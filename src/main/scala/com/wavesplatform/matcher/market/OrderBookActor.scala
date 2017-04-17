package com.wavesplatform.matcher.market

import akka.actor.Props
import akka.http.scaladsl.model.StatusCodes
import akka.persistence._
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.{CancelOrderRequest, MatcherResponse}
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model.Events.{Event, OrderAdded, OrderExecuted}
import com.wavesplatform.matcher.model.MatcherModel._
import com.wavesplatform.matcher.model.{OrderValidator, _}
import play.api.libs.json.Json
import scorex.crypto.encode.Base58
import scorex.transaction.TransactionModule
import scorex.transaction.assets.exchange._
import scorex.transaction.state.database.blockchain.StoredState
import scorex.utils.{NTP, ScorexLogging}
import scorex.wallet.Wallet

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global

class OrderBookActor(assetPair: AssetPair, val storedState: StoredState,
                     val wallet: Wallet, val settings: MatcherSettings,
                     val transactionModule: TransactionModule)
  extends PersistentActor
    with ScorexLogging with OrderValidator with OrderHistory with ExchangeTransactionCreator {
  override def persistenceId: String = OrderBookActor.name(assetPair)

  private var orderBook = OrderBook.empty

  context.system.scheduler.schedule(settings.snapshotsInterval, settings.snapshotsInterval, self, SaveSnapshot)

  override def postStop(): Unit = {
    log.info(context.self.toString() + " - postStop method")
  }

  override def receiveCommand: Receive = {
    case order: Order =>
      handleAddOrder(order)
    case GetOrdersRequest =>
      sender() ! GetOrdersResponse(orderBook.asks.values.flatten.toSeq ++ orderBook.bids.values.flatten.toSeq)
    case GetAskOrdersRequest =>
      sender() ! GetOrdersResponse(orderBook.asks.values.flatten.toSeq)
    case GetBidOrdersRequest =>
      sender() ! GetOrdersResponse(orderBook.bids.values.flatten.toSeq)
    case GetOrderBookRequest(pair, depth) =>
      handleGetOrderBook(pair, depth)
    case SaveSnapshot =>
      deleteSnapshots(SnapshotSelectionCriteria.Latest)
      saveSnapshot(Snapshot(orderBook, ordersRemainingAmount.cache.asMap().asScala.toMap))
    case SaveSnapshotSuccess(metadata) =>
      log.info(s"Snapshot saved with metadata $metadata")
    case SaveSnapshotFailure(metadata, reason) =>
      log.error(s"Failed to save snapshot: $metadata, $reason.")
    case GetOrderStatus(_, id) =>
      handleOrderStatus(id)
    case cancel: CancelOrder =>
      handleCancelOrder(cancel)
  }

  def handleOrderStatus(id: String): Unit = {
    sender() ! GetOrderStatusResponse(getOrderStatus(id))
  }

  private def handleCancelOrder(cancel: CancelOrder) = {
    val v = validateCancelOrder(cancel)
    if (v) {
      OrderBook.cancelOrder(orderBook, cancel.orderId) match {
        case Some(oc) if cancel.req.senderPublicKey == oc.limitOrder.order.senderPublicKey =>
          persist(oc) { _ =>
            handleCancelEvent(oc)
            sender() ! OrderCanceled(cancel.orderId)
          }
        case _ => sender() ! OrderCancelRejected("Order not found")
      }
    } else {
      sender() ! OrderCancelRejected(v.messages())
    }
  }

  def handleGetOrderBook(pair: AssetPair, depth: Option[Int]): Unit = {
    def aggregateLevel(l: (Price, Level[LimitOrder])) = LevelAgg(l._1, l._2.foldLeft(0L)((b, o) => b + o.amount))

    if (pair == assetPair) {
      val d = Math.min(depth.getOrElse(MaxDepth), MaxDepth)
      sender() ! GetOrderBookResponse(pair, orderBook.bids.take(d).map(aggregateLevel).toSeq,
        orderBook.asks.take(d).map(aggregateLevel).toSeq)
    } else sender() ! GetOrderBookResponse(pair, Seq(), Seq())
  }

  override def receiveRecover: Receive = {
    case evt: Event => log.debug("Event: {}", evt); applyEvent(evt)
    case RecoveryCompleted => log.info(assetPair.toString() + " - Recovery completed!");
    case SnapshotOffer(_, snapshot: Snapshot) =>
      log.debug(s"Recovering OrderBook from snapshot: $snapshot for $persistenceId")
      orderBook = snapshot.orderBook
      recoverFromOrderBook(orderBook)
      initOrdersCache(snapshot.history)
  }

  def handleAddOrder(order: Order): Unit = {
    val v = validateNewOrder(order)
    if (v) {
      sender() ! OrderAccepted(order)
      matchOrder(LimitOrder(order))
    } else {
      sender() ! OrderRejected(v.messages())
    }
  }

  def applyEvent(e: Event): Unit = {
    orderBook = OrderBook.updateState(orderBook, e)
    e match {
      case OrderAdded(o) => didOrderAccepted(o)
      case e: OrderExecuted => didOrderExecuted(e)
      case e: Events.OrderCanceled => didOrderCanceled(e)
    }
  }

  @tailrec
  private def matchOrder(limitOrder: LimitOrder): Unit = {
    val remOrder = handleMatchEvent(OrderBook.matchOrder(orderBook, limitOrder))
    if (remOrder.isDefined) matchOrder(remOrder.get)
  }

  def handleMatchEvent(e: Event): Option[LimitOrder] = {
    def processEvent(e: Event) = {
      persist(e)(_ => ())
      applyEvent(e)
      context.system.eventStream.publish(e)
    }

    e match {
      case e: OrderAdded =>
        processEvent(e)
        None

      case e@OrderExecuted(o, c) =>
        val txVal = createTransaction(o, c)
        txVal match {
          case Right(tx) if isValid(tx) =>
            sendToNetwork(tx)
            processEvent(e)
            if (e.submittedRemaining > 0)
              Some(o.partial(e.submittedRemaining))
            else None
          case _ =>
            val canceled = Events.OrderCanceled(c)
            processEvent(canceled)
            Some(o)
        }
      case _ => None
    }
  }

  def handleCancelEvent(e: Event): Unit = {
    applyEvent(e)
    context.system.eventStream.publish(e)
  }

}

object OrderBookActor {
  def props(assetPair: AssetPair, storedState: StoredState,
            wallet: Wallet, settings: MatcherSettings, transactionModule: TransactionModule): Props =
    Props(new OrderBookActor(assetPair, storedState, wallet, settings, transactionModule))

  def name(assetPair: AssetPair): String = assetPair.toString

  val MaxDepth = 50

  //protocol
  sealed trait OrderBookRequest {
    def assetPair: AssetPair
  }

  case class GetOrderBookRequest(assetPair: AssetPair, depth: Option[Int]) extends OrderBookRequest

  case class GetOrderStatus(assetPair: AssetPair, id: String) extends OrderBookRequest

  case class CancelOrder(assetPair: AssetPair, req: CancelOrderRequest) extends OrderBookRequest {
    def orderId: String = Base58.encode(req.orderId)
  }

  case class OrderAccepted(order: Order) extends MatcherResponse {
    val json = Json.obj("status" -> "OrderAccepted", "message" -> order.json)
    val code = StatusCodes.OK
  }

  case class OrderRejected(message: String) extends MatcherResponse {
    val json = Json.obj("status" -> "OrderRejected", "message" -> message)
    val code = StatusCodes.BadRequest
  }

  case class OrderCanceled(orderId: String) extends MatcherResponse {
    val json = Json.obj("status" -> "OrderCanceled", "orderId" -> orderId)
    val code = StatusCodes.OK
  }

  case class OrderCancelRejected(message: String) extends MatcherResponse {
    val json = Json.obj("status" -> "OrderCancelRejected", "message" -> message)
    val code = StatusCodes.BadRequest
  }

  case class GetOrderStatusResponse(status: LimitOrder.OrderStatus) extends MatcherResponse {
    val json = status.json
    val code = StatusCodes.OK
  }

  case class GetOrderBookResponse(pair: AssetPair, bids: Seq[LevelAgg], asks: Seq[LevelAgg]) extends MatcherResponse {
    val json = Json.toJson(OrderBookResult(NTP.correctedTime(), pair, bids, asks))
    val code = StatusCodes.OK
  }

  // Direct requests
  case object GetOrdersRequest

  case object GetBidOrdersRequest

  case object GetAskOrdersRequest

  case class GetOrdersResponse(orders: Seq[LimitOrder])

  case object SaveSnapshot

  case class Snapshot(orderBook: OrderBook, history: Map[String, (Long, Long)])

  val bidsOrdering: Ordering[Long] = (x: Long, y: Long) => -Ordering.Long.compare(x, y)
  val asksOrdering: Ordering[Long] = (x: Long, y: Long) => Ordering.Long.compare(x, y)
}


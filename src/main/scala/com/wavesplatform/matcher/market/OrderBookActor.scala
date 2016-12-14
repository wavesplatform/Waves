package com.wavesplatform.matcher.market

import akka.actor.Props
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import akka.persistence._
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model.Events.{Event, OrderAdded, OrderExecuted}
import com.wavesplatform.matcher.model.MatcherModel._
import com.wavesplatform.matcher.model.{OrderValidator, _}
import com.wavesplatform.matcher.util.Cache
import com.wavesplatform.settings.WavesSettings
import play.api.libs.json.{JsString, JsValue, Json}
import scorex.crypto.encode.Base58
import scorex.transaction.SimpleTransactionModule._
import scorex.transaction.TransactionModule
import scorex.transaction.assets.exchange._
import scorex.transaction.state.database.blockchain.StoredState
import scorex.utils.{NTP, ScorexLogging}
import scorex.wallet.Wallet

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.collection.JavaConversions._

class OrderBookActor(assetPair: AssetPair, val storedState: StoredState,
                     val wallet: Wallet, val settings: WavesSettings,
                     val transactionModule: TransactionModule[StoredInBlock])
  extends PersistentActor
  with ScorexLogging with OrderValidator with OrderHistory with OrderMatchCreator {
  override def persistenceId: String = OrderBookActor.name(assetPair)

  private var orderBook = OrderBook.empty
  private var restoreState = true

  context.system.scheduler.schedule(settings.snapshotInterval, settings.snapshotInterval, self, SaveSnapshot)

  override def receiveCommand: Receive = {
    case order:Order =>
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
        saveSnapshot(Snapshot(orderBook, ordersRemainingAmount.cache.asMap().toMap))
    case SaveSnapshotSuccess(metadata) =>
      log.info(s"Snapshot saved with metadata $metadata")
    case SaveSnapshotFailure(metadata, reason) =>
      log.error(s"Failed to save snapshot: $metadata, $reason.")
    case GetOrderStatus(_, id) =>
      handleOrderStatus(id)
    case CancelOrder(_, tx) =>
      handleCancelOrder(tx)
  }

  def handleOrderStatus(id: String): Unit = {
    sender() ! GetOrderStatusResponse(getOrderStatus(id))
  }

  def handleCancelOrder(tx: OrderCancelTransaction) = {
    val v = validateCancelOrder(tx)
    if (v) {
      persist(OrderBook.cancelOrder(orderBook, tx.orderIdStr)) { e =>
        sender() ! OrderCanceled(tx.orderIdStr)
        e match {
          case Some(c) => handleCancelEvent(c, tx)
          case None => sender() ! OrderCancelRejected("Order not found")
        }
      }
    } else {
      sender() ! OrderCancelRejected(v.messages)
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
    case SnapshotOffer(metadata, snapshot: Snapshot) =>
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
      sender() ! OrderRejected(v.messages)
    }
  }

  def applyEvent(e: Event): Unit = {
    orderBook = OrderBook.updateState(orderBook, e)
    e match {
      case OrderAdded(o) => didOrderAccepted(o)
      case e: OrderExecuted => didOrderExecuted(e)
      case e: Events.OrderCanceled => didOrderCanceled(e)
      case _ =>
    }
  }

  def matchOrder(limitOrder: LimitOrder): Unit = {
    persist(OrderBook.matchOrder(orderBook, limitOrder)) { e =>
      handleMatchEvent(e).foreach(matchOrder)
    }
  }

  def handleMatchEvent(e: Event): Option[LimitOrder] = {
    applyEvent(e)
    context.system.eventStream.publish(e)

    e match {
      case e: OrderAdded =>
        None
      case e@OrderExecuted(o, c) =>
        val tx = createTransaction(o, c)
        if (isValid(tx)) {
          sendToNetwork(tx)
        }
        if (e.submittedRemaining > 0) Some(o.partial(e.submittedRemaining))
        else None
      case _ => None
    }
  }

  def handleCancelEvent(e: Event, tx: OrderCancelTransaction): Unit = {
    applyEvent(e)
    context.system.eventStream.publish(e)

    sendToNetwork(tx)
  }

}

object OrderBookActor {
  def props(assetPair: AssetPair, storedState: StoredState,
            wallet: Wallet, settings: WavesSettings, transactionModule: TransactionModule[StoredInBlock]): Props =
    Props(new OrderBookActor(assetPair, storedState, wallet, settings, transactionModule))
  def name(assetPair: AssetPair): String = assetPair.first.map(Base58.encode).getOrElse("WAVES") + "-" +
    assetPair.second.map(Base58.encode).getOrElse("WAVES")

  val MaxDepth = 50

  //protocol
  sealed trait OrderBookRequest {
    def pair: AssetPair
  }
  case class GetOrderBookRequest(pair: AssetPair, depth: Option[Int]) extends OrderBookRequest
  case class GetOrderStatus(pair: AssetPair, id: String) extends OrderBookRequest
  case class CancelOrder(pair: AssetPair, tx: OrderCancelTransaction) extends OrderBookRequest

  sealed trait OrderBookResponse {
    def json: JsValue
    def code: StatusCode
  }

  sealed trait OrderResponse extends OrderBookResponse
  case class OrderAccepted(order: Order) extends OrderResponse {
    val json = Json.obj("status" -> "OrderAccepted", "message" -> order.json)
    val code = StatusCodes.OK
  }

  case class OrderRejected(message: String) extends OrderResponse {
    val json = Json.obj("status" -> "OrderRejected", "message" -> message)
    val code = StatusCodes.BadRequest
  }

  case class OrderCanceled(orderId: String) extends OrderResponse {
    val json = Json.obj("status" -> "OrderCanceled", "orderId" -> orderId)
    val code = StatusCodes.OK
  }

  case class OrderCancelRejected(message: String) extends OrderResponse {
    val json = Json.obj("status" -> "OrderCancelRejected", "message" -> message)
    val code = StatusCodes.BadRequest
  }

  case object NotFoundPair extends OrderBookResponse {
    val json = JsString("Unknown Assets Pair")
    val code = StatusCodes.NotFound
  }

  case class GetOrderStatusResponse(status: LimitOrder.OrderStatus) extends OrderBookResponse {
    val json = status.json
    val code = StatusCodes.OK
  }

  case class GetOrderBookResponse(pair: AssetPair, bids: Seq[LevelAgg], asks: Seq[LevelAgg]) extends OrderBookResponse {
    val json = Json.toJson(OrderBookResult(NTP.correctedTime(), pair, bids, asks))
    val code = StatusCodes.OK
  }

  // Direct requests
  case object GetOrdersRequest
  case object GetBidOrdersRequest
  case object GetAskOrdersRequest
  case class GetOrdersResponse(orders: Seq[LimitOrder])

  case object SaveSnapshot

  @SerialVersionUID(-5350485695558994597L)
  case class Snapshot(orderBook: OrderBook, history: Map[String, (Long, Long)])

  val bidsOrdering: Ordering[Long] = new Ordering[Long] {
    def compare(x: Long, y: Long): Int = - Ordering.Long.compare(x, y)
  }

  val asksOrdering: Ordering[Long] = new Ordering[Long] {
    def compare(x: Long, y: Long): Int = Ordering.Long.compare(x, y)
  }
}


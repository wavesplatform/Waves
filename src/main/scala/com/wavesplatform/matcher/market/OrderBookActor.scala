package com.wavesplatform.matcher.market

import akka.actor.{ActorRef, Cancellable, Props, Stash}
import akka.http.scaladsl.model.StatusCodes
import akka.persistence._
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.{CancelOrderRequest, MatcherResponse}
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.market.OrderHistoryActor._
import com.wavesplatform.matcher.model.Events.{Event, OrderAdded, OrderExecuted}
import com.wavesplatform.matcher.model.MatcherModel._
import com.wavesplatform.matcher.model.{OrderValidator, _}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import play.api.libs.json._
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError.CustomError
import scorex.transaction.assets.exchange._
import scorex.transaction.{History, NewTransactionHandler}
import scorex.utils.{NTP, ScorexLogging}
import scorex.wallet.Wallet

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class OrderBookActor(assetPair: AssetPair, val orderHistory: ActorRef,
                     val storedState: StateReader,
                     val wallet: Wallet, val settings: MatcherSettings,
                     val history: History,
                     val functionalitySettings: FunctionalitySettings,
                     val transactionModule: NewTransactionHandler)
  extends PersistentActor
    with Stash with ScorexLogging with ExchangeTransactionCreator {
  override def persistenceId: String = OrderBookActor.name(assetPair)

  private var orderBook = OrderBook.empty

  context.system.scheduler.schedule(settings.snapshotsInterval, settings.snapshotsInterval, self, SaveSnapshot)

  override def postStop(): Unit = {
    log.info(context.self.toString() + " - postStop method")
  }

  var apiSender = Option.empty[ActorRef]
  var cancellable = Option.empty[Cancellable]

  override def receiveCommand: Receive = fullCommands

  def fullCommands: Receive = readOnlyCommands orElse snapshotsCommands orElse executeCommands

  def executeCommands: Receive =  {
    case order: Order =>
      onAddOrder(order)
    case cancel: CancelOrder =>
      onCancelOrder(cancel)
  }

  def snapshotsCommands: Receive = {
    case SaveSnapshot =>
      deleteSnapshots(SnapshotSelectionCriteria.Latest)
      saveSnapshot(Snapshot(orderBook))
    case SaveSnapshotSuccess(metadata) =>
      log.info(s"Snapshot saved with metadata $metadata")
    case SaveSnapshotFailure(metadata, reason) =>
      log.error(s"Failed to save snapshot: $metadata, $reason.")
    case DeleteOrderBookRequest(pair) =>
      deleteMessages(lastSequenceNr)
      deleteSnapshots(SnapshotSelectionCriteria.Latest)
      context.stop(self)
      sender() ! GetOrderBookResponse(pair, Seq(), Seq())
  }

  def waitingValidation: Receive = readOnlyCommands orElse {
    case ValidationTimeoutExceeded =>
      log.warn("Validation timeout exceeded, skip incoming request")
      becomeFullCommands()
    case ValidateOrderResult(res) =>
      cancellable.foreach(_.cancel())
      handleValidateOrderResult(res)
    case ValidateCancelResult(res) =>
      cancellable.foreach(_.cancel())
      handleValidateCancelResult(res)
    case ev =>
      log.info("Stashed: " + ev)
      stash()
  }

  def readOnlyCommands: Receive = {
    case GetOrdersRequest =>
      sender() ! GetOrdersResponse(orderBook.asks.values.flatten.toSeq ++ orderBook.bids.values.flatten.toSeq)
    case GetAskOrdersRequest =>
      sender() ! GetOrdersResponse(orderBook.asks.values.flatten.toSeq)
    case GetBidOrdersRequest =>
      sender() ! GetOrdersResponse(orderBook.bids.values.flatten.toSeq)
    case GetOrderBookRequest(pair, depth) =>
      handleGetOrderBook(pair, depth)
  }

  def onCancelOrder(cancel: CancelOrder): Unit = {
    orderHistory ! ValidateCancelOrder(cancel)
    apiSender = Some(sender())
    cancellable = Some(context.system.scheduler.scheduleOnce(ValidationTimeout, self, ValidationTimeoutExceeded))
    context.become(waitingValidation)
  }

  def handleValidateCancelResult(res: Either[CustomError, CancelOrder]): Unit = {
    res match {
      case Left(err) =>
        apiSender.foreach(_ ! OrderCancelRejected(err.s))
      case Right(cancel) =>
        OrderBook.cancelOrder(orderBook, cancel.orderId) match {
          case Some(oc) =>
            persist(oc) { _ =>
              handleCancelEvent(oc)
              apiSender.foreach(_ ! OrderCanceled(cancel.orderId))
            }
          case _ => apiSender.foreach(_ ! OrderCancelRejected("Order not found"))
        }
    }

    becomeFullCommands()
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
    case evt: Event =>
      log.debug("Event: {}", evt)
      applyEvent(evt)
      if (settings.isMigrateToNewOrderHistoryStorage) {
        orderHistory ! evt
      }
    case RecoveryCompleted => log.info(assetPair.toString() + " - Recovery completed!");
    case SnapshotOffer(_, snapshot: Snapshot) =>
      orderBook = snapshot.orderBook
      if (settings.isMigrateToNewOrderHistoryStorage) {
        orderHistory ! RecoverFromOrderBook(orderBook)
      }
      log.debug(s"Recovering OrderBook from snapshot: $snapshot for $persistenceId")
  }

  def onAddOrder(order: Order): Unit = {
    orderHistory ! ValidateOrder(order)
    apiSender = Some(sender())
    cancellable = Some(context.system.scheduler.scheduleOnce(ValidationTimeout, self, ValidationTimeoutExceeded))
    context.become(waitingValidation)
  }

  def handleValidateOrderResult(res: Either[CustomError, Order]): Unit = {
    res match {
      case Left(err) =>
        log.debug(s"Order rejected: $err.err")
        apiSender.foreach(_ ! OrderRejected(err.s))
      case Right(o) =>
        log.debug(s"Order accepted: ${o.idStr}, trying to match ...")
        apiSender.foreach(_ ! OrderAccepted(o))
        matchOrder(LimitOrder(o))
    }

    becomeFullCommands()
  }

  def becomeFullCommands(): Unit = {
    unstashAll()
    context.become(fullCommands)
  }

  def applyEvent(e: Event): Unit = {
    orderBook = OrderBook.updateState(orderBook, e)
  }

  @tailrec
  private def matchOrder(limitOrder: LimitOrder): Unit = {
    val remOrder = handleMatchEvent(OrderBook.matchOrder(orderBook, limitOrder))
    if (remOrder.isDefined) {
      if (LimitOrder.validateAmount(remOrder.get) && LimitOrder.validateIntegerAmount(storedState, remOrder.get)) {
        matchOrder(remOrder.get)
      } else {
        val canceled = Events.OrderCanceled(remOrder.get)
        processEvent(canceled)
      }
    }
  }

  private def processEvent(e: Event) = {
    persist(e)(_ => ())
    applyEvent(e)
    context.system.eventStream.publish(e)
  }

  def handleMatchEvent(e: Event): Option[LimitOrder] = {
    e match {
      case e: OrderAdded =>
        processEvent(e)
        None

      case e@OrderExecuted(o, c) =>
        val result = for {
          transaction <- createTransaction(o, c)
          validationResult <- validate(transaction)
          sendResult <- sendToNetwork(validationResult)
        } yield sendResult

        result match {
          case Left(ex) =>
            log.debug(s"Failed to execute order: $ex")
            val canceled = Events.OrderCanceled(c)
            processEvent(canceled)
            Some(o)
          case Right(_) =>
            processEvent(e)
            if (e.submittedRemaining > 0)
              Some(o.partial(e.submittedRemaining))
            else None
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
  def props(assetPair: AssetPair, orderHistory: ActorRef,  storedState: StateReader, settings: MatcherSettings, wallet: Wallet, transactionModule: NewTransactionHandler, history: History,
            functionalitySettings: FunctionalitySettings): Props =
    Props(new OrderBookActor(assetPair, orderHistory, storedState, wallet, settings, history, functionalitySettings, transactionModule))

  def name(assetPair: AssetPair): String = assetPair.toString

  val MaxDepth = 50
  val ValidationTimeout = 3.seconds

  //protocol
  sealed trait OrderBookRequest {
    def assetPair: AssetPair
  }

  case class GetOrderBookRequest(assetPair: AssetPair, depth: Option[Int]) extends OrderBookRequest

  case class DeleteOrderBookRequest(assetPair: AssetPair) extends OrderBookRequest

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

  case class GetOrderHistoryResponse(history: Seq[(String, OrderInfo, Option[Order])]) extends MatcherResponse {
    val json = JsArray(history.map(h => Json.obj(
      "id" -> h._1,
      "type" -> h._3.map(_.orderType.toString),
      "amount" -> h._2.amount,
      "price" -> h._3.map(_.price),
      "timestamp" -> h._3.map(_.timestamp),
      "filled" -> h._2.filled,
      "status" -> h._2.status.name
      )))
    val code = StatusCodes.OK
  }

  case class GetOrderBookResponse(pair: AssetPair, bids: Seq[LevelAgg], asks: Seq[LevelAgg]) extends MatcherResponse {
    val json: JsValue = Json.toJson(OrderBookResult(NTP.correctedTime(), pair, bids, asks))
    val code = StatusCodes.OK
  }

  // Direct requests
  case object GetOrdersRequest

  case object GetBidOrdersRequest

  case object GetAskOrdersRequest

  case class GetOrdersResponse(orders: Seq[LimitOrder])

  case object SaveSnapshot

  case class Snapshot(orderBook: OrderBook)

  case object ValidationTimeoutExceeded
}


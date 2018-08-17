package com.wavesplatform.matcher.market

import akka.actor.{ActorRef, Cancellable, Props, Stash}
import akka.http.scaladsl.model.StatusCodes
import akka.persistence._
import com.google.common.cache.CacheBuilder
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.MatcherResponse
import com.wavesplatform.matcher.market.MatcherActor.{Shutdown, ShutdownComplete}
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.market.OrderHistoryActor._
import com.wavesplatform.matcher.model.Events.{Event, ExchangeTransactionCreated, OrderAdded, OrderExecuted}
import com.wavesplatform.matcher.model.MatcherModel.{Level, Price}
import com.wavesplatform.matcher.model._
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.network._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.{Blockchain, ByteStr}
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.{AccountBalanceError, GenericError, NegativeAmount, OrderValidationError}
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.utils.{NTP, ScorexLogging}
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import play.api.libs.json._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global

class OrderBookActor(assetPair: AssetPair,
                     updateSnapshot: OrderBook => Unit,
                     val orderHistory: ActorRef,
                     val blockchain: Blockchain,
                     val wallet: Wallet,
                     val utx: UtxPool,
                     val allChannels: ChannelGroup,
                     val settings: MatcherSettings,
                     val functionalitySettings: FunctionalitySettings)
    extends PersistentActor
    with Stash
    with ScorexLogging
    with ExchangeTransactionCreator {
  override def persistenceId: String = OrderBookActor.name(assetPair)

  private val timer       = Kamon.timer("matcher.orderbook.match").refine("pair"    -> assetPair.toString)
  private val cancelTimer = Kamon.timer("matcher.orderbook.persist").refine("event" -> "OrderCancelled")
  private val validationTimeouts = Kamon
    .counter("matcher.orderbook.error")
    .refine(
      "pair"  -> assetPair.toString,
      "group" -> "validation",
      "type"  -> "timeout"
    )

  private val snapshotCancellable    = context.system.scheduler.schedule(settings.snapshotsInterval, settings.snapshotsInterval, self, SaveSnapshot)
  private val cleanupCancellable     = context.system.scheduler.schedule(settings.orderCleanupInterval, settings.orderCleanupInterval, self, OrderCleanup)
  private var orderBook              = OrderBook.empty
  private var apiSender              = Option.empty[ActorRef]
  private var cancellable            = Option.empty[Cancellable]
  private var lastSnapshotSequenceNr = 0L

  private var shutdownStatus: ShutdownStatus = ShutdownStatus(
    initiated = false,
    oldMessagesDeleted = false,
    oldSnapshotsDeleted = false,
    onComplete = () => ()
  )

  private var lastTrade: Option[Order] = None

  private lazy val alreadyCanceledOrders = CacheBuilder
    .newBuilder()
    .maximumSize(AlreadyCanceledCacheSize)
    .build[ByteStr, java.lang.Boolean]()

  private lazy val cancelInProgressOrders = CacheBuilder
    .newBuilder()
    .maximumSize(AlreadyCanceledCacheSize)
    .build[ByteStr, java.lang.Boolean]()

  val okCancel: java.lang.Boolean     = Boolean.box(true)
  val failedCancel: java.lang.Boolean = Boolean.box(false)

  private def fullCommands: Receive = readOnlyCommands orElse snapshotsCommands orElse executeCommands

  private def executeCommands: Receive = {
    case order: Order =>
      onAddOrder(order)
    case cancel: CancelOrder =>
      onCancelOrder(cancel)
    case ForceCancelOrder(_, orderId) =>
      log.trace(s"Force cancel order $orderId")
      onForceCancelOrder(orderId)
    case OrderCleanup =>
      onOrderCleanup(orderBook, NTP.correctedTime())
  }

  private def snapshotsCommands: Receive = {
    case SaveSnapshot =>
      saveSnapshot(Snapshot(orderBook))

    case SaveSnapshotSuccess(metadata) =>
      lastSnapshotSequenceNr = metadata.sequenceNr
      log.info(s"Snapshot saved with metadata $metadata")
      deleteMessages(metadata.sequenceNr)
      deleteSnapshots(SnapshotSelectionCriteria.Latest.copy(maxSequenceNr = metadata.sequenceNr - 1))

    case SaveSnapshotFailure(metadata, reason) =>
      log.error(s"Failed to save snapshot: $metadata, $reason.")
      if (shutdownStatus.initiated) {
        shutdownStatus = shutdownStatus.copy(
          oldSnapshotsDeleted = true,
          oldMessagesDeleted = true
        )
        shutdownStatus.tryComplete()
      }

    case DeleteOrderBookRequest(pair) =>
      updateSnapshot(OrderBook.empty)
      orderBook.asks.values
        .++(orderBook.bids.values)
        .flatten
        .foreach(x => context.system.eventStream.publish(Events.OrderCanceled(x, unmatchable = false)))
      deleteMessages(lastSequenceNr)
      deleteSnapshots(SnapshotSelectionCriteria.Latest)
      sender() ! GetOrderBookResponse(pair, Seq(), Seq())
      context.stop(self)

    case DeleteSnapshotsSuccess(criteria) =>
      log.info(s"$persistenceId DeleteSnapshotsSuccess with $criteria")
      if (shutdownStatus.initiated) {
        shutdownStatus = shutdownStatus.copy(oldSnapshotsDeleted = true)
        shutdownStatus.tryComplete()
      }

    case DeleteSnapshotsFailure(criteria, cause) =>
      log.error(s"$persistenceId DeleteSnapshotsFailure with $criteria, reason: $cause")
      if (shutdownStatus.initiated) {
        shutdownStatus = shutdownStatus.copy(oldSnapshotsDeleted = true)
        shutdownStatus.tryComplete()
      }

    case DeleteMessagesSuccess(toSequenceNr) =>
      log.info(s"$persistenceId DeleteMessagesSuccess up to $toSequenceNr")
      if (shutdownStatus.initiated) {
        shutdownStatus = shutdownStatus.copy(oldMessagesDeleted = true)
        shutdownStatus.tryComplete()
      }

    case DeleteMessagesFailure(cause: Throwable, toSequenceNr: Long) =>
      log.error(s"$persistenceId DeleteMessagesFailure up to $toSequenceNr, reason: $cause")
      if (shutdownStatus.initiated) {
        shutdownStatus = shutdownStatus.copy(oldMessagesDeleted = true)
        shutdownStatus.tryComplete()
      }

    case Shutdown =>
      if (!shutdownStatus.initiated) {
        val s = sender()
        shutdownStatus = shutdownStatus.copy(initiated = true, onComplete = { () =>
          s ! ShutdownComplete
          context.stop(self)
        })

        if (lastSnapshotSequenceNr < lastSequenceNr) saveSnapshot(Snapshot(orderBook))
        else {
          log.debug(s"No changes in $assetPair, lastSnapshotSequenceNr = $lastSnapshotSequenceNr, lastSequenceNr = $lastSequenceNr")
          shutdownStatus = shutdownStatus.copy(
            oldSnapshotsDeleted = true,
            oldMessagesDeleted = true
          )
          shutdownStatus.tryComplete()
        }
      }
  }

  private def waitingValidation(sentMessage: Either[ValidateCancelOrder, ValidateOrder]): Receive = readOnlyCommands orElse {
    case ValidationTimeoutExceeded =>
      validationTimeouts.increment()
      log.warn(s"Validation timeout exceeded for $sentMessage")
      val orderId = sentMessage.fold(_.cancel.orderId, x => ByteStr(x.order.id()))
      cancelInProgressOrders.invalidate(orderId)
      apiSender.foreach(_ ! OperationTimedOut)
      becomeFullCommands()

    case ValidateOrderResult(validatedOrderId, res) =>
      sentMessage match {
        case Right(sent) if sent.order.id().sameElements(validatedOrderId.arr) =>
          cancellable.foreach(_.cancel())
          handleValidateOrderResult(res)
        case x =>
          log.warn(s"Unexpected message: $x")
      }

    case ValidateCancelResult(validatedOrderId, res) =>
      sentMessage match {
        case Left(sent) if validatedOrderId == sent.cancel.orderId =>
          cancellable.foreach(_.cancel())
          handleValidateCancelResult(res.map(x => x.orderId))
        case x =>
          log.warn(s"Unexpected message: $x")
      }

    case cancel: CancelOrder if Option(cancelInProgressOrders.getIfPresent(cancel.orderId)).nonEmpty =>
      log.info(s"Order($assetPair, ${cancel.orderId}) is already being canceled")
      sender() ! OrderCancelRejected("Order is already being canceled")

    case ev =>
      log.trace("Stashed: " + ev)
      stash()
  }

  private def readOnlyCommands: Receive = {
    case GetOrdersRequest =>
      sender() ! GetOrdersResponse(orderBook.asks.values.flatten.toSeq ++ orderBook.bids.values.flatten.toSeq)
    case GetAskOrdersRequest =>
      sender() ! GetOrdersResponse(orderBook.asks.values.flatten.toSeq)
    case GetBidOrdersRequest =>
      sender() ! GetOrdersResponse(orderBook.bids.values.flatten.toSeq)
    case GetMarketStatusRequest(pair) =>
      handleGetMarketStatus(pair)
  }

  private def onCancelOrder(cancel: CancelOrder): Unit = {
    Option(alreadyCanceledOrders.getIfPresent(cancel.orderId)) match {
      case Some(`okCancel`) =>
        log.info(s"Order($assetPair, ${cancel.orderId}) is already canceled")
        sender() ! OrderCanceled(cancel.orderId.base58)
      case Some(_) =>
        log.info(s"Order($assetPair, ${cancel.orderId}) is already not found")
        sender() ! OrderCancelRejected("Order not found")
      case None =>
        val msg = ValidateCancelOrder(cancel, NTP.correctedTime())
        orderHistory ! msg
        apiSender = Some(sender())
        cancellable = Some(context.system.scheduler.scheduleOnce(settings.validationTimeout, self, ValidationTimeoutExceeded))
        context.become(waitingValidation(Left(msg)))
        cancelInProgressOrders.put(cancel.orderId, okCancel)
    }
  }

  private def onForceCancelOrder(orderIdToCancel: ByteStr): Unit = {
    OrderBook.cancelOrder(orderBook, orderIdToCancel) match {
      case Some(oc) =>
        val st = cancelTimer.start()
        persist(oc) { _ =>
          applyEvent(oc)
          sender() ! OrderCanceled(orderIdToCancel.base58)
          st.stop()
        }
      case _ => sender() ! OrderCancelRejected("Order not found")
    }
  }

  private def onOrderCleanup(orderBook: OrderBook, ts: Long): Unit = {
    orderBook.asks.values
      .++(orderBook.bids.values)
      .flatten
      .filterNot(x => {
        val validation = x.order.isValid(ts)
        validation
      })
      .map(_.order.id())
      .foreach(x => handleValidateCancelResult(Right(ByteStr(x))))
  }

  private def handleValidateCancelResult(res: Either[GenericError, ByteStr]): Unit = {
    res match {
      case Left(err) =>
        apiSender.foreach(_ ! OrderCancelRejected(err.err))
      case Right(orderIdToCancel) =>
        cancelInProgressOrders.invalidate(orderIdToCancel)
        OrderBook.cancelOrder(orderBook, orderIdToCancel) match {
          case Some(oc) =>
            val st = cancelTimer.start()
            alreadyCanceledOrders.put(orderIdToCancel, okCancel)
            persist(oc) { _ =>
              handleCancelEvent(oc)
              apiSender.foreach(_ ! OrderCanceled(orderIdToCancel.base58))
              st.stop()
            }
          case _ =>
            alreadyCanceledOrders.put(orderIdToCancel, failedCancel)
            apiSender.foreach(_ ! OrderCancelRejected("Order not found"))
        }
    }

    becomeFullCommands()
  }

  private def handleGetMarketStatus(pair: AssetPair): Unit = {
    if (pair == assetPair)
      sender() ! GetMarketStatusResponse(pair, orderBook.bids.headOption, orderBook.asks.headOption, lastTrade)
    else
      sender() ! GetMarketStatusResponse(pair, None, None, None)
  }

  private def onAddOrder(order: Order): Unit = {
    val msg = ValidateOrder(order, NTP.correctedTime())
    orderHistory ! msg
    apiSender = Some(sender())
    cancellable = Some(context.system.scheduler.scheduleOnce(settings.validationTimeout, self, ValidationTimeoutExceeded))
    context.become(waitingValidation(Right(msg)))
  }

  private def handleValidateOrderResult(res: Either[GenericError, Order]): Unit = {
    res match {
      case Left(err) =>
        log.debug(s"Order rejected: $err.err")
        apiSender.foreach(_ ! OrderRejected(err.err))
      case Right(o) =>
        log.debug(s"Order accepted: '${o.idStr()}' in '${o.assetPair.key}', trying to match ...")
        apiSender.foreach(_ ! OrderAccepted(o))
        timer.measure(matchOrder(LimitOrder(o)))
    }

    becomeFullCommands()
  }

  private def becomeFullCommands(): Unit = {
    unstashAll()
    context.become(fullCommands)
  }

  private def applyEvent(e: Event): Unit = {
    orderBook = OrderBook.updateState(orderBook, e)
    updateSnapshot(orderBook)
  }

  @tailrec
  private def matchOrder(limitOrder: LimitOrder): Unit = {
    val (submittedRemains, counterRemains) = handleMatchEvent(OrderBook.matchOrder(orderBook, limitOrder))
    if (counterRemains.isDefined) {
      if (!counterRemains.get.isValid) {
        val canceled = Events.OrderCanceled(counterRemains.get, unmatchable = true)
        processEvent(canceled)
      }
    }
    if (submittedRemains.isDefined) {
      if (submittedRemains.get.isValid) {
        matchOrder(submittedRemains.get)
      } else {
        val canceled = Events.OrderCanceled(submittedRemains.get, unmatchable = true)
        processEvent(canceled)
      }
    }
  }

  private def processEvent(e: Event): Unit = {
    val st = Kamon.timer("matcher.orderbook.persist").refine("event" -> e.getClass.getSimpleName).start()
    persist(e)(_ => st.stop())
    applyEvent(e)
    context.system.eventStream.publish(e)
  }

  private def processInvalidTransaction(event: OrderExecuted, err: ValidationError): Option[LimitOrder] = {
    def cancelCounterOrder(): Option[LimitOrder] = {
      processEvent(Events.OrderCanceled(event.counter, unmatchable = false))
      Some(event.submitted)
    }

    log.debug(s"Failed to execute order: $err")
    err match {
      case OrderValidationError(order, _) if order == event.submitted.order => None
      case OrderValidationError(order, _) if order == event.counter.order   => cancelCounterOrder()
      case AccountBalanceError(errs) =>
        errs.foreach(e => log.error(s"Balance error: ${e._2}"))
        if (errs.contains(event.counter.order.senderPublicKey)) {
          cancelCounterOrder()
        }
        if (errs.contains(event.submitted.order.senderPublicKey)) {
          None
        } else {
          Some(event.submitted)
        }
      case _: NegativeAmount =>
        processEvent(Events.OrderCanceled(event.submitted, unmatchable = true))
        None
      case _ =>
        cancelCounterOrder()
    }
  }

  private def handleMatchEvent(e: Event): (Option[LimitOrder], Option[LimitOrder]) = {
    e match {
      case e: OrderAdded =>
        processEvent(e)
        (None, None)

      case event @ OrderExecuted(o, c) =>
        (for {
          tx <- createTransaction(o, c)
          _  <- utx.putIfNew(tx)
        } yield tx) match {
          case Right(tx) =>
            lastTrade = Some(c.order)
            allChannels.broadcastTx(tx)
            processEvent(event)
            context.system.eventStream.publish(ExchangeTransactionCreated(tx))
            (
              if (event.submittedRemainingAmount <= 0) None
              else
                Some(
                  o.partial(
                    event.submittedRemainingAmount,
                    event.submittedRemainingFee
                  )
                ),
              if (event.counterRemainingAmount <= 0) None
              else
                Some(
                  c.partial(
                    event.counterRemainingAmount,
                    event.counterRemainingFee
                  )
                )
            )
          case Left(ex) =>
            log.info(s"Can't create tx: $ex\no1: ${Json.prettyPrint(o.order.json())}\no2: ${Json.prettyPrint(c.order.json())}")
            (processInvalidTransaction(event, ex), None)
        }

      case _ => (None, None)
    }
  }

  private def handleCancelEvent(e: Event): Unit = {
    applyEvent(e)
    context.system.eventStream.publish(e)
  }

  override def receiveCommand: Receive = fullCommands

  val isMigrateToNewOrderHistoryStorage: Boolean = settings.isMigrateToNewOrderHistoryStorage

  override def receiveRecover: Receive = {
    case evt: Event =>
      applyEvent(evt)
      if (isMigrateToNewOrderHistoryStorage) {
        orderHistory ! evt
      }

    case RecoveryCompleted =>
      log.info(assetPair.toString() + " - Recovery completed!")
      updateSnapshot(orderBook)

    case SnapshotOffer(metadata, snapshot: Snapshot) =>
      lastSnapshotSequenceNr = metadata.sequenceNr
      orderBook = snapshot.orderBook
      updateSnapshot(orderBook)
      if (isMigrateToNewOrderHistoryStorage) {
        orderHistory ! RecoverFromOrderBook(orderBook)
      }
      log.debug(s"Recovering OrderBook from snapshot: $snapshot for $persistenceId")
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.warn(s"Restarting actor because of $message", reason)
    super.preRestart(reason, message)
  }

  override def postStop(): Unit = {
    log.info(context.self.toString() + " - postStop method")
    snapshotCancellable.cancel()
    cleanupCancellable.cancel()
    cancellable.foreach(_.cancel())
  }
}

object OrderBookActor {
  def props(assetPair: AssetPair,
            updateSnapshot: OrderBook => Unit,
            orderHistory: ActorRef,
            blockchain: Blockchain,
            settings: MatcherSettings,
            wallet: Wallet,
            utx: UtxPool,
            allChannels: ChannelGroup,
            functionalitySettings: FunctionalitySettings): Props =
    Props(new OrderBookActor(assetPair, updateSnapshot, orderHistory, blockchain, wallet, utx, allChannels, settings, functionalitySettings))

  def name(assetPair: AssetPair): String = assetPair.toString

  val MaxDepth                 = 50
  val AlreadyCanceledCacheSize = 10000L

  private case class ShutdownStatus(initiated: Boolean, oldMessagesDeleted: Boolean, oldSnapshotsDeleted: Boolean, onComplete: () => Unit) {
    def completed: Boolean  = initiated && oldMessagesDeleted && oldSnapshotsDeleted
    def tryComplete(): Unit = if (completed) onComplete()
  }

  //protocol
  sealed trait OrderBookRequest {
    def assetPair: AssetPair
  }

  case class GetOrderBookRequest(assetPair: AssetPair, depth: Option[Int]) extends OrderBookRequest

  case class GetMarketStatusRequest(assetPair: AssetPair) extends OrderBookRequest

  case class DeleteOrderBookRequest(assetPair: AssetPair) extends OrderBookRequest

  case class CancelOrder(assetPair: AssetPair, sender: PublicKeyAccount, orderId: ByteStr) extends OrderBookRequest {
    override lazy val toString: String = s"CancelOrder($assetPair, $sender, $orderId)"
  }

  case class ForceCancelOrder(assetPair: AssetPair, orderId: ByteStr) extends OrderBookRequest

  case object OrderCleanup

  case object OperationTimedOut extends MatcherResponse {
    val json = Json.obj("status" -> "OperationTimedOut", "message" -> "Operation is timed out, please try later")
    val code = StatusCodes.InternalServerError
  }

  case class OrderAccepted(order: Order) extends MatcherResponse {
    val json: JsObject            = Json.obj("status" -> "OrderAccepted", "message" -> order.json())
    val code: StatusCodes.Success = StatusCodes.OK
  }

  case class OrderRejected(message: String) extends MatcherResponse {
    val json: JsObject                = Json.obj("status" -> "OrderRejected", "message" -> message)
    val code: StatusCodes.ClientError = StatusCodes.BadRequest
  }

  case class OrderCanceled(orderId: String) extends MatcherResponse {
    val json: JsObject            = Json.obj("status" -> "OrderCanceled", "orderId" -> orderId)
    val code: StatusCodes.Success = StatusCodes.OK
  }

  case class OrderCancelRejected(message: String) extends MatcherResponse {
    val json: JsObject                = Json.obj("status" -> "OrderCancelRejected", "message" -> message)
    val code: StatusCodes.ClientError = StatusCodes.BadRequest
  }

  case class GetOrderBookResponse(pair: AssetPair, bids: Seq[LevelAgg], asks: Seq[LevelAgg]) extends MatcherResponse {
    val json: JsValue             = Json.toJson(OrderBookResult(NTP.correctedTime(), pair, bids, asks))
    val code: StatusCodes.Success = StatusCodes.OK
  }

  object GetOrderBookResponse {
    def empty(pair: AssetPair): GetOrderBookResponse = GetOrderBookResponse(pair, Seq(), Seq())
  }

  case class GetMarketStatusResponse(pair: AssetPair,
                                     bid: Option[(Price, Level[LimitOrder])],
                                     ask: Option[(Price, Level[LimitOrder])],
                                     last: Option[Order])
      extends MatcherResponse {
    def json: JsValue =
      Json.obj(
        "lastPrice" -> last.map(_.price),
        "lastSide"  -> last.map(_.orderType.toString),
        "bid"       -> bid.map(_._1),
        "bidAmount" -> bid.map(_._2.map(_.amount).sum),
        "ask"       -> ask.map(_._1),
        "askAmount" -> ask.map(_._2.map(_.amount).sum)
      )
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

package com.wavesplatform.matcher.market

import akka.actor.{ActorRef, Cancellable, Props, Stash}
import akka.http.scaladsl.model._
import akka.persistence._
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api._
import com.wavesplatform.matcher.market.MatcherActor.{Shutdown, ShutdownComplete}
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.market.OrderHistoryActor._
import com.wavesplatform.matcher.model.Events.{Event, ExchangeTransactionCreated, OrderAdded}
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

  val okCancel: java.lang.Boolean     = Boolean.box(true)
  val failedCancel: java.lang.Boolean = Boolean.box(false)

  private def fullCommands: Receive = readOnlyCommands orElse snapshotsCommands orElse executeCommands

  private def executeCommands: Receive = {
    case order: Order        => onAddOrder(order)
    case cancel: CancelOrder => onCancelOrder(cancel.orderId)
    case OrderCleanup        => onOrderCleanup(orderBook, NTP.correctedTime())
  }

  private def snapshotsCommands: Receive = {
    case SaveSnapshot =>
      saveSnapshot(Snapshot(orderBook))

    case SaveSnapshotSuccess(metadata) =>
      lastSnapshotSequenceNr = metadata.sequenceNr
      deleteMessages(metadata.sequenceNr)
      deleteSnapshots(SnapshotSelectionCriteria.Latest.copy(maxSequenceNr = metadata.sequenceNr - 1))

    case SaveSnapshotFailure(metadata, reason) =>
      log.error(s"Failed to save snapshot: $metadata", reason)
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
      sender() ! GetOrderBookResponse(NTP.correctedTime(), pair, Seq(), Seq())
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
          shutdownStatus = shutdownStatus.copy(
            oldSnapshotsDeleted = true,
            oldMessagesDeleted = true
          )
          shutdownStatus.tryComplete()
        }
      }
  }

  private def waitingValidation(sentMessage: ValidateOrder): Receive = readOnlyCommands orElse {
    case ValidationTimeoutExceeded =>
      validationTimeouts.increment()
      log.warn(s"Validation timeout exceeded for $sentMessage")
      apiSender.foreach(_ ! OperationTimedOut)
      becomeFullCommands()

    case ValidateOrderResult(validatedOrderId, res) =>
      if (validatedOrderId == sentMessage.order.id()) {
        cancellable.foreach(_.cancel())
        handleValidateOrderResult(validatedOrderId, res)
      } else {
        log.warn(s"Unexpected ValidateOrderResult for order $validatedOrderId while waiting for ${sentMessage.order.id()}")
      }

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

  private def onOrderCleanup(orderBook: OrderBook, ts: Long): Unit = {
    orderBook.asks.values
      .++(orderBook.bids.values)
      .flatten
      .filterNot(x => {
        val validation = x.order.isValid(ts)
        validation
      })
      .map(_.order.id())
      .foreach(onCancelOrder)
  }

  private def onCancelOrder(orderIdToCancel: ByteStr): Unit =
    OrderBook.cancelOrder(orderBook, orderIdToCancel) match {
      case Some(oc) =>
        val st = cancelTimer.start()
        persist(oc) { _ =>
          handleCancelEvent(oc)
          sender() ! OrderCanceled(orderIdToCancel)
          st.stop()
        }
      case _ =>
        log.debug(s"Error cancelling $orderIdToCancel: order not found")
        sender() ! OrderCancelRejected("Order not found")
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
    context.become(waitingValidation(msg))
  }

  private def handleValidateOrderResult(orderId: ByteStr, res: Either[GenericError, Order]): Unit = {
    res match {
      case Left(err) =>
        log.debug(s"Order $orderId rejected: ${err.err}")
        apiSender.foreach(_ ! OrderRejected(err.err))
      case Right(o) =>
        log.debug(s"Order accepted: '${o.idStr()}' in '${o.assetPair.key}', trying to match ...")
        timer.measure(matchOrder(LimitOrder(o)))
        apiSender.foreach(_ ! OrderAccepted(o))
    }

    becomeFullCommands()
  }

  private def becomeFullCommands(): Unit = {
    unstashAll()
    context.become(fullCommands)
  }

  private def applyEvent(e: Event): Unit = {
    log.debug(s"Apply event $e")
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

  private def processInvalidTransaction(event: Events.OrderExecuted, err: ValidationError): Option[LimitOrder] = {
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
      case e: Events.OrderAdded =>
        processEvent(e)
        (None, None)

      case event @ Events.OrderExecuted(o, c) =>
        (for {
          tx <- createTransaction(event)
          _  <- utx.putIfNew(tx)
        } yield tx) match {
          case Right(tx) =>
            lastTrade = Some(o.order)
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
            log.info(s"""Can't create tx: $ex
                 |o1: (amount=${o.amount}, fee=${o.fee}): ${Json.prettyPrint(o.order.json())}
                 |o2: (amount=${c.amount}, fee=${c.fee}): ${Json.prettyPrint(c.order.json())}""".stripMargin)
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

  override def receiveRecover: Receive = {
    case evt: Event =>
      applyEvent(evt)

    case RecoveryCompleted =>
      updateSnapshot(orderBook)
      log.debug(s"Recovery completed: $orderBook")

      if (settings.recoverOrderHistory) {
        val orders = (orderBook.asks.valuesIterator ++ orderBook.bids.valuesIterator).flatten
        if (orders.nonEmpty) {
          val ids = orders.map { limitOrder =>
            context.system.eventStream.publish(OrderAdded(limitOrder))
            limitOrder.order.id()
          }.toSeq

          log.info(s"Recovering an order history for orders: ${ids.mkString(", ")}")
        }
      }

    case SnapshotOffer(metadata, snapshot: Snapshot) =>
      lastSnapshotSequenceNr = metadata.sequenceNr
      orderBook = snapshot.orderBook
      updateSnapshot(orderBook)
      log.debug(s"Recovering $persistenceId from $snapshot")
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.warn(s"Restarting actor because of $message", reason)
    super.preRestart(reason, message)
  }

  override def postStop(): Unit = {
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

  private case class ShutdownStatus(initiated: Boolean, oldMessagesDeleted: Boolean, oldSnapshotsDeleted: Boolean, onComplete: () => Unit) {
    def completed: Boolean  = initiated && oldMessagesDeleted && oldSnapshotsDeleted
    def tryComplete(): Unit = if (completed) onComplete()
  }

  case class GetOrderBookRequest(assetPair: AssetPair, depth: Option[Int])

  case class GetMarketStatusRequest(assetPair: AssetPair)

  case class DeleteOrderBookRequest(assetPair: AssetPair)

  case class CancelOrder(orderId: ByteStr)

  case object OrderCleanup

  case class GetOrderBookResponse(ts: Long, pair: AssetPair, bids: Seq[LevelAgg], asks: Seq[LevelAgg]) {
    def toHttpResponse: HttpResponse = HttpResponse(
      entity = HttpEntity(
        ContentTypes.`application/json`,
        JsonSerializer.serialize(OrderBookResult(ts, pair, bids, asks))
      )
    )
  }

  object GetOrderBookResponse {
    def empty(pair: AssetPair): GetOrderBookResponse = GetOrderBookResponse(NTP.correctedTime(), pair, Seq(), Seq())
  }

  case class GetMarketStatusResponse(pair: AssetPair,
                                     bid: Option[(Price, Level[LimitOrder])],
                                     ask: Option[(Price, Level[LimitOrder])],
                                     last: Option[Order])
      extends MatcherResponse(
        StatusCodes.OK,
        Json.obj(
          "lastPrice" -> last.map(_.price),
          "lastSide"  -> last.map(_.orderType.toString),
          "bid"       -> bid.map(_._1),
          "bidAmount" -> bid.map(_._2.map(_.amount).sum),
          "ask"       -> ask.map(_._1),
          "askAmount" -> ask.map(_._2.map(_.amount).sum)
        )
      )

  // Direct requests
  case object GetOrdersRequest

  case object GetBidOrdersRequest

  case object GetAskOrdersRequest

  case class GetOrdersResponse(orders: Seq[LimitOrder])

  case object SaveSnapshot

  case class Snapshot(orderBook: OrderBook)

  case object ValidationTimeoutExceeded
}

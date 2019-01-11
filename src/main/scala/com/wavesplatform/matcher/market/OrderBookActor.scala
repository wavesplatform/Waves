package com.wavesplatform.matcher.market

import akka.actor.{ActorRef, Props, Status}
import akka.http.scaladsl.model._
import akka.persistence._
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api._
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model.Events.{Event, ExchangeTransactionCreated, OrderAdded, OrderExecuted}
import com.wavesplatform.matcher.model.MatcherModel.{Level, Price}
import com.wavesplatform.matcher.model._
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.network._
import com.wavesplatform.state.ByteStr
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.{AccountBalanceError, HasScriptType, NegativeAmount, OrderValidationError}
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.utils.{ScorexLogging, Time}
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import play.api.libs.json._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global

class OrderBookActor(parent: ActorRef,
                     assetPair: AssetPair,
                     updateSnapshot: OrderBook => Unit,
                     updateMarketStatus: MarketStatus => Unit,
                     utx: UtxPool,
                     allChannels: ChannelGroup,
                     settings: MatcherSettings,
                     createTransaction: OrderExecuted => Either[ValidationError, ExchangeTransaction],
                     time: Time)
    extends PersistentActor
    with ScorexLogging {

  override def persistenceId: String = OrderBookActor.name(assetPair)

  private val matchTimer         = Kamon.timer("matcher.orderbook.match").refine("pair" -> assetPair.toString)
  private val persistCancelTimer = Kamon.timer("matcher.orderbook.persist").refine("event" -> "OrderCancelled")
  private val cancelTimer        = Kamon.timer("matcher.orderbook.cancel")

  private val cleanupCancellable = context.system.scheduler.schedule(settings.orderCleanupInterval, settings.orderCleanupInterval, self, OrderCleanup)
  private var orderBook          = OrderBook.empty

  private var lastMarketStatus = MarketStatus(assetPair, orderBook.bids.headOption, orderBook.asks.headOption, None)
  private def refreshMarketStatus(newLastOrder: Option[Order] = None): Unit = {
    lastMarketStatus = lastMarketStatus.copy(
      bid = orderBook.bids.headOption,
      ask = orderBook.asks.headOption,
      last = newLastOrder.orElse(lastMarketStatus.last)
    )
    updateMarketStatus(lastMarketStatus)
  }

  private def fullCommands: Receive = readOnlyCommands orElse snapshotsCommands orElse executeCommands

  private def executeCommands: Receive = {
    case order: Order        => onAddOrder(order)
    case cancel: CancelOrder => onCancelOrder(cancel.orderId)
    case OrderCleanup        => onOrderCleanup(orderBook, time.correctedTime())
  }

  private def snapshotsCommands: Receive = {
    case SaveSnapshot =>
      log.info("Starting saving a snapshot")
      saveSnapshot(Snapshot(orderBook))

    case SaveSnapshotSuccess(metadata) =>
      log.info(s"Snapshot has been saved: $metadata")
      deleteMessages(metadata.sequenceNr)
      deleteSnapshots(SnapshotSelectionCriteria.Latest.copy(maxSequenceNr = metadata.sequenceNr - 1))

    case SaveSnapshotFailure(metadata, reason) =>
      log.error(s"Failed to save snapshot: $metadata", reason)

    case _: DeleteOrderBookRequest =>
      updateSnapshot(OrderBook.empty)
      orderBook.asks.values
        .++(orderBook.bids.values)
        .flatten
        .foreach(x => context.system.eventStream.publish(Events.OrderCanceled(x, unmatchable = false)))
      deleteMessages(lastSequenceNr)
      deleteSnapshots(SnapshotSelectionCriteria.Latest)
      sender() ! Status.Success(0)
      context.stop(self)

    case DeleteSnapshotsSuccess(criteria) =>
      log.debug(s"$persistenceId DeleteSnapshotsSuccess with $criteria")

    case DeleteSnapshotsFailure(criteria, cause) =>
      log.error(s"$persistenceId DeleteSnapshotsFailure with $criteria, reason: $cause")

    case DeleteMessagesSuccess(toSequenceNr) =>
      log.debug(s"$persistenceId DeleteMessagesSuccess up to $toSequenceNr")

    case DeleteMessagesFailure(cause: Throwable, toSequenceNr: Long) =>
      log.error(s"$persistenceId DeleteMessagesFailure up to $toSequenceNr, reason: $cause")
  }

  private def readOnlyCommands: Receive = {
    case GetOrdersRequest =>
      sender() ! GetOrdersResponse(orderBook.asks.values.flatten.toSeq ++ orderBook.bids.values.flatten.toSeq)
    case GetAskOrdersRequest =>
      sender() ! GetOrdersResponse(orderBook.asks.values.flatten.toSeq)
    case GetBidOrdersRequest =>
      sender() ! GetOrdersResponse(orderBook.bids.values.flatten.toSeq)
  }

  private def onOrderCleanup(orderBook: OrderBook, ts: Long): Unit = {
    orderBook.asks.values
      .++(orderBook.bids.values)
      .flatten
      .filterNot { x =>
        val validation = x.order.isValid(ts)
        validation
      }
      .map(_.order.id())
      .foreach(onCancelOrder)
  }

  private def onCancelOrder(orderIdToCancel: ByteStr): Unit =
    OrderBook.cancelOrder(orderBook, orderIdToCancel) match {
      case Some(oc) =>
        val st = persistCancelTimer.start()
        persist(oc) { _ =>
          st.stop()
          cancelTimer.measure(handleCancelEvent(oc))
          sender() ! OrderCanceled(orderIdToCancel)
        }
      case _ =>
        log.debug(s"Error cancelling $orderIdToCancel: order not found")
        sender() ! OrderCancelRejected("Order not found")
    }

  private def onAddOrder(order: Order): Unit = {
    log.trace(s"Order accepted: '${order.id()}' in '${order.assetPair.key}', trying to match ...")
    matchTimer.measure(matchOrder(LimitOrder(order)))
    sender() ! OrderAccepted(order)
  }

  private def applyEvent(e: Event): Unit = {
    log.trace(s"Apply event $e")
    log.info(e match {
      case Events.OrderAdded(order) => s"OrderAdded(${order.order.idStr()}, amount=${order.amount})"
      case exec @ Events.OrderExecuted(submitted, counter) =>
        s"OrderExecuted(s=${submitted.order.idStr()}, c=${counter.order.idStr()}, amount=${exec.executedAmount})"
      case Events.OrderCanceled(order, unmatchable) => s"OrderCanceled(${order.order.idStr()}, system=$unmatchable)"
    })
    orderBook = OrderBook.updateState(orderBook, e)
    refreshMarketStatus()
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
    persist(e) { _ =>
      st.stop()
      if (lastSequenceNr % settings.snapshotsInterval == 0) self ! SaveSnapshot
    }
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
      case TransactionValidationError(x: HasScriptType, _) if x.isTokenScript =>
        processEvent(Events.OrderCanceled(event.counter, unmatchable = false))
        processEvent(Events.OrderCanceled(event.submitted, unmatchable = false))
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
            refreshMarketStatus(Some(o.order))
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
      if (settings.recoverOrderHistory) context.system.eventStream.publish(evt)

    case RecoveryCompleted =>
      updateSnapshot(orderBook)
      log.debug(s"Recovery completed: $orderBook")
      if (settings.makeSnapshotsAtStart) self ! SaveSnapshot

    case SnapshotOffer(_, snapshot: Snapshot) =>
      orderBook = snapshot.orderBook
      refreshMarketStatus()
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

      log.debug(s"Recovering $persistenceId from $snapshot")
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.warn(s"Restarting actor because of $message", reason)
    super.preRestart(reason, message)
  }

  override def postStop(): Unit = {
    cleanupCancellable.cancel()
  }
}

object OrderBookActor {
  def props(parent: ActorRef,
            assetPair: AssetPair,
            updateSnapshot: OrderBook => Unit,
            updateMarketStatus: MarketStatus => Unit,
            utx: UtxPool,
            allChannels: ChannelGroup,
            settings: MatcherSettings,
            createTransaction: OrderExecuted => Either[ValidationError, ExchangeTransaction],
            time: Time): Props =
    Props(new OrderBookActor(parent, assetPair, updateSnapshot, updateMarketStatus, utx, allChannels, settings, createTransaction, time))

  def name(assetPair: AssetPair): String = assetPair.toString

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
    def empty(pair: AssetPair): GetOrderBookResponse = GetOrderBookResponse(System.currentTimeMillis(), pair, Seq(), Seq())
  }

  case class MarketStatus(pair: AssetPair, bid: Option[(Price, Level[LimitOrder])], ask: Option[(Price, Level[LimitOrder])], last: Option[Order])

  // Direct requests
  case object GetOrdersRequest

  case object GetBidOrdersRequest

  case object GetAskOrdersRequest

  case class GetOrdersResponse(orders: Seq[LimitOrder])

  case object SaveSnapshot

  case class Snapshot(orderBook: OrderBook)

}

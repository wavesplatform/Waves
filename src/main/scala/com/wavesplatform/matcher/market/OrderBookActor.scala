package com.wavesplatform.matcher.market

import akka.actor.{ActorRef, Props}
import akka.persistence._
import com.wavesplatform.matcher._
import com.wavesplatform.matcher.api._
import com.wavesplatform.matcher.market.MatcherActor.SaveSnapshot
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model.Events.{Event, ExchangeTransactionCreated, OrderAdded}
import com.wavesplatform.matcher.model.ExchangeTransactionCreator.CreateTransaction
import com.wavesplatform.matcher.model.MatcherModel.{Level, Price}
import com.wavesplatform.matcher.model._
import com.wavesplatform.matcher.queue.{QueueEvent, QueueEventWithMeta}
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.state.ByteStr
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.{AccountBalanceError, HasScriptType, NegativeAmount, OrderValidationError}
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.utils.{LoggerFacade, ScorexLogging, Time}
import kamon.Kamon
import org.slf4j.LoggerFactory
import play.api.libs.json._

import scala.annotation.tailrec

class OrderBookActor(owner: ActorRef,
                     addressActor: ActorRef,
                     assetPair: AssetPair,
                     updateSnapshot: OrderBook => Unit,
                     updateMarketStatus: MarketStatus => Unit,
                     broadcastTx: ExchangeTransaction => Unit,
                     settings: MatcherSettings,
                     createTransaction: CreateTransaction,
                     time: Time)
    extends PersistentActor
    with ScorexLogging {

  override def persistenceId: String = OrderBookActor.name(assetPair)

  protected override def log = LoggerFacade(LoggerFactory.getLogger(s"OrderBookActor[${assetPair.key}]"))

  private var savingSnapshot: Option[QueueEventWithMeta.Offset] = None
  private var lastProcessedOffset: Long                         = -1L

  private val matchTimer = Kamon.timer("matcher.orderbook.match").refine("pair" -> assetPair.toString)
  private var orderBook  = OrderBook.empty

  private var lastTrade = Option.empty[LastTrade]

  private def refreshMarketStatus(lastEvent: Option[Event] = None): Unit = {
    lastEvent.foreach {
      case x: Events.OrderExecuted => lastTrade = Some(LastTrade(x.counter.price, x.executedAmount, x.submitted.order.orderType))
      case _                       => // no need to update last trade
    }

    updateMarketStatus(MarketStatus(lastTrade, orderBook.bids.headOption, orderBook.asks.headOption))
  }

  private def fullCommands: Receive = readOnlyCommands orElse executeCommands orElse snapshotsCommands

  private def executeCommands: Receive = {
    case request: QueueEventWithMeta =>
      if (request.offset <= lastProcessedOffset) sender() ! AlreadyProcessed
      else {
        lastProcessedOffset = request.offset
        request.event match {
          case x: QueueEvent.Placed   => onAddOrder(request, x.order)
          case x: QueueEvent.Canceled => onCancelOrder(request.offset, x.orderId)
          case _: QueueEvent.OrderBookDeleted =>
            sender() ! GetOrderBookResponse(OrderBookResult(time.correctedTime(), assetPair, Seq(), Seq()))
            updateSnapshot(OrderBook.empty)
            orderBook.asks.values
              .++(orderBook.bids.values)
              .flatten
              .foreach(x => publishEvent(Events.OrderCanceled(x, unmatchable = false)))
            context.stop(self)
        }
      }
  }

  private def snapshotsCommands: Receive = {
    case SaveSnapshotSuccess(metadata) =>
      val snapshotOffsetId = savingSnapshot.getOrElse(throw new IllegalStateException("Impossible"))
      log.info(s"Snapshot has been saved at offset $snapshotOffsetId: $metadata")
      owner ! OrderBookSnapshotUpdated(assetPair, snapshotOffsetId)
      savingSnapshot = None

    case SaveSnapshotFailure(metadata, reason) =>
      savingSnapshot = None
      log.error(s"Failed to save snapshot: $metadata", reason)

    case SaveSnapshot(globalEventNr) =>
      if (savingSnapshot.isEmpty) {
        saveSnapshotAt(globalEventNr)
        savingSnapshot = Some(globalEventNr)
      }
  }

  private def readOnlyCommands: Receive = {
    case GetOrdersRequest =>
      sender() ! GetOrdersResponse(orderBook.asks.values.flatten.toSeq ++ orderBook.bids.values.flatten.toSeq)
    case GetAskOrdersRequest =>
      sender() ! GetOrdersResponse(orderBook.asks.values.flatten.toSeq)
    case GetBidOrdersRequest =>
      sender() ! GetOrdersResponse(orderBook.bids.values.flatten.toSeq)
  }

  private def onCancelOrder(requestId: QueueEventWithMeta.Offset, orderIdToCancel: ByteStr): Unit = sender() ! {
    OrderBook.cancelOrder(orderBook, orderIdToCancel) match {
      case Some(oc) =>
        handleCancelEvent(oc)
        OrderCanceled(orderIdToCancel)
      case _ =>
        log.debug(s"Error cancelling $orderIdToCancel: order not found")
        OrderCancelRejected("Order not found")
    }
  }

  private def onAddOrder(eventWithMeta: QueueEventWithMeta, order: Order): Unit = {
    log.trace(s"Order accepted: '${order.id()}', trying to match ...")
    matchTimer.measure(matchOrder(eventWithMeta, LimitOrder(order)))
    sender() ! OrderAccepted(order) // TODO respond immediately
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
    refreshMarketStatus(Some(e))
    updateSnapshot(orderBook)
  }

  @tailrec
  private def matchOrder(eventWithMeta: QueueEventWithMeta, limitOrder: LimitOrder): Unit = {
    val (submittedRemains, counterRemains) = handleMatchEvent(eventWithMeta, OrderBook.matchOrder(orderBook, limitOrder))
    if (counterRemains.isDefined) {
      if (!counterRemains.get.isValid) {
        val canceled = Events.OrderCanceled(counterRemains.get, unmatchable = true)
        processEvent(canceled)
      }
    }
    if (submittedRemains.isDefined) {
      if (submittedRemains.get.isValid) {
        matchOrder(eventWithMeta, submittedRemains.get)
      } else {
        val canceled = Events.OrderCanceled(submittedRemains.get, unmatchable = true)
        processEvent(canceled)
      }
    }
  }

  private def processEvent(e: Event): Unit = {
    applyEvent(e)
    publishEvent(e)
  }

  private def processInvalidTransaction(event: Events.OrderExecuted, err: ValidationError): Option[LimitOrder] = {
    def cancelCounterOrder(): Option[LimitOrder] = {
      processEvent(Events.OrderCanceled(event.counter, unmatchable = false))
      Some(event.submitted)
    }

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

  private def handleMatchEvent(eventWithMeta: QueueEventWithMeta, e: Event): (Option[LimitOrder], Option[LimitOrder]) = {
    e match {
      case e: Events.OrderAdded =>
        processEvent(e)
        (None, None)

      case event @ Events.OrderExecuted(o, c) =>
        createTransaction(o, c, eventWithMeta.timestamp) match {
          case Right(tx) =>
            broadcastTx(tx)
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
    publishEvent(e)
  }

  override def receiveCommand: Receive = fullCommands

  override def receiveRecover: Receive = {
    case RecoveryCompleted =>
      updateSnapshot(orderBook)
      owner ! OrderBookSnapshotUpdated(assetPair, lastProcessedOffset)
      log.debug(s"Recovery completed: $orderBook")

    case SnapshotOffer(_, snapshot: Snapshot) =>
      orderBook = snapshot.orderBook
      lastProcessedOffset = snapshot.eventNr

      refreshMarketStatus()
      for (level <- orderBook.asks.valuesIterator ++ orderBook.bids.valuesIterator; lo <- level) {
        publishEvent(OrderAdded(lo))
      }

      log.debug(s"Recovering from $snapshot")
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.warn(s"Restarting actor because of $message", reason)
    super.preRestart(reason, message)
  }

  private def publishEvent(e: Event): Unit = {
    addressActor ! e
    context.system.eventStream.publish(e)
  }

  private def saveSnapshotAt(globalEventNr: QueueEventWithMeta.Offset): Unit =
    saveSnapshot(Snapshot(globalEventNr, orderBook))
}

object OrderBookActor {
  def props(parent: ActorRef,
            addressActor: ActorRef,
            assetPair: AssetPair,
            updateSnapshot: OrderBook => Unit,
            updateMarketStatus: MarketStatus => Unit,
            broadcastTx: ExchangeTransaction => Unit,
            settings: MatcherSettings,
            createTransaction: CreateTransaction,
            time: Time): Props =
    Props(new OrderBookActor(parent, addressActor, assetPair, updateSnapshot, updateMarketStatus, broadcastTx, settings, createTransaction, time))

  def name(assetPair: AssetPair): String = assetPair.toString

  case class MarketStatus(
      lastTrade: Option[LastTrade],
      bestBid: Option[(Price, Level[LimitOrder])],
      bestAsk: Option[(Price, Level[LimitOrder])],
  )

  object MarketStatus {
    implicit val fmt: Writes[MarketStatus] = { ms =>
      val b = ms.bestBid.map(aggregateLevel)
      val a = ms.bestAsk.map(aggregateLevel)
      Json.obj(
        "lastPrice"  -> ms.lastTrade.map(_.price),
        "lastAmount" -> ms.lastTrade.map(_.amount),
        "lastSide"   -> ms.lastTrade.map(_.side.toString),
        "bid"        -> b.map(_.price),
        "bidAmount"  -> b.map(_.amount),
        "ask"        -> a.map(_.price),
        "askAmount"  -> a.map(_.amount)
      )
    }
  }

  case class LastTrade(price: Long, amount: Long, side: OrderType)

  // Direct requests
  case object GetOrdersRequest

  case object GetBidOrdersRequest

  case object GetAskOrdersRequest

  case class GetOrdersResponse(orders: Seq[LimitOrder])

  case class Snapshot(eventNr: Long, orderBook: OrderBook)

  // Internal messages
  case class OrderBookSnapshotUpdated(assetPair: AssetPair, eventNr: Long)
}

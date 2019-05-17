package com.wavesplatform.matcher.market

import akka.actor.{ActorRef, Props}
import akka.persistence._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.api._
import com.wavesplatform.matcher.market.MatcherActor.{ForceStartOrderBook, OrderBookCreated, SaveSnapshot}
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model.Events.{Event, ExchangeTransactionCreated, OrderAdded}
import com.wavesplatform.matcher.model.ExchangeTransactionCreator.CreateTransaction
import com.wavesplatform.matcher.model._
import com.wavesplatform.matcher.queue.{QueueEvent, QueueEventWithMeta}
import com.wavesplatform.matcher.settings.MatcherSettings
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.utils.{LoggerFacade, ScorexLogging, Time}
import kamon.Kamon
import org.slf4j.LoggerFactory
import play.api.libs.json._

class OrderBookActor(owner: ActorRef,
                     addressActor: ActorRef,
                     assetPair: AssetPair,
                     updateSnapshot: OrderBook.AggregatedSnapshot => Unit,
                     updateMarketStatus: MarketStatus => Unit,
                     createTransaction: CreateTransaction,
                     time: Time,
                     normalizedTickSize: Option[Long] = None)
    extends PersistentActor
    with ScorexLogging {

  override def persistenceId: String = OrderBookActor.name(assetPair)

  protected override val log = LoggerFacade(LoggerFactory.getLogger(s"OrderBookActor[$assetPair]"))

  private var savingSnapshot          = Option.empty[QueueEventWithMeta.Offset]
  private var lastSavedSnapshotOffset = Option.empty[QueueEventWithMeta.Offset]
  private var lastProcessedOffset     = Option.empty[QueueEventWithMeta.Offset]

  private val addTimer    = Kamon.timer("matcher.orderbook.add").refine("pair" -> assetPair.toString)
  private val cancelTimer = Kamon.timer("matcher.orderbook.cancel").refine("pair" -> assetPair.toString)
  private var orderBook   = OrderBook.empty

  private var lastTrade = Option.empty[LastTrade]

  private def fullCommands: Receive = executeCommands orElse snapshotsCommands

  private def executeCommands: Receive = {
    case request: QueueEventWithMeta =>
      lastProcessedOffset match {
        case Some(lastProcessed) if request.offset <= lastProcessed => sender() ! AlreadyProcessed
        case _ =>
          lastProcessedOffset = Some(request.offset)
          request.event match {
            case x: QueueEvent.Placed   => onAddOrder(request, x.order)
            case x: QueueEvent.Canceled => onCancelOrder(request, x.orderId)
            case _: QueueEvent.OrderBookDeleted =>
              sender() ! GetOrderBookResponse(OrderBookResult(time.correctedTime(), assetPair, Seq(), Seq()))
              updateSnapshot(OrderBook.AggregatedSnapshot())
              processEvents(orderBook.cancelAll(request.timestamp))
              context.stop(self)
          }
      }
    case ForceStartOrderBook(p) if p == assetPair =>
      sender() ! OrderBookCreated(assetPair)
  }

  private def snapshotsCommands: Receive = {
    case SaveSnapshotSuccess(metadata) =>
      val snapshotOffsetId = savingSnapshot.getOrElse(throw new IllegalStateException("Impossible"))
      log.info(s"Snapshot has been saved at offset $snapshotOffsetId: $metadata")
      owner ! OrderBookSnapshotUpdated(assetPair, snapshotOffsetId)
      lastSavedSnapshotOffset = Some(snapshotOffsetId)
      savingSnapshot = None

    case SaveSnapshotFailure(metadata, reason) =>
      savingSnapshot = None
      log.error(s"Failed to save snapshot: $metadata", reason)

    case SaveSnapshot(globalEventNr) =>
      if (savingSnapshot.isEmpty && lastSavedSnapshotOffset.getOrElse(-1L) < globalEventNr) {
        log.debug(s"About to save snapshot $orderBook")
        saveSnapshotAt(globalEventNr)
        savingSnapshot = Some(globalEventNr)
      }
  }

  private def processEvents(events: Iterable[Event]): Unit = {
    events.foreach { e =>
      e match {
        case Events.OrderAdded(order, _) =>
          log.info(s"OrderAdded(${order.order.id()}, amount=${order.amount})")
        case x @ Events.OrderExecuted(submitted, counter, timestamp) =>
          log.info(s"OrderExecuted(s=${submitted.order.idStr()}, c=${counter.order.idStr()}, amount=${x.executedAmount})")
          lastTrade = Some(LastTrade(counter.price, x.executedAmount, x.submitted.order.orderType))
          createTransaction(submitted, counter, timestamp) match {
            case Right(tx) => context.system.eventStream.publish(ExchangeTransactionCreated(tx))
            case Left(ex) =>
              log.warn(s"""Can't create tx: $ex
                          |o1: (amount=${submitted.amount}, fee=${submitted.fee}): ${Json.prettyPrint(submitted.order.json())}
                          |o2: (amount=${counter.amount}, fee=${counter.fee}): ${Json.prettyPrint(counter.order.json())}""".stripMargin)
          }
        case Events.OrderCanceled(order, unmatchable, _) =>
          log.info(s"OrderCanceled(${order.order.idStr()}, system=$unmatchable)")
      }

      addressActor ! e
    }

    updateMarketStatus(MarketStatus(lastTrade, orderBook.bestBid, orderBook.bestAsk))
    updateSnapshot(orderBook.aggregatedSnapshot)
  }

  private def onCancelOrder(event: QueueEventWithMeta, orderIdToCancel: ByteStr): Unit =
    cancelTimer.measure(orderBook.cancel(orderIdToCancel, event.timestamp) match {
      case Some(cancelEvent) =>
        processEvents(List(cancelEvent))
      case None =>
        log.warn(s"Error applying $event: order not found")
    })

  private def onAddOrder(eventWithMeta: QueueEventWithMeta, order: Order): Unit = addTimer.measure {
    log.trace(s"Applied $eventWithMeta, trying to match ...")
    processEvents(orderBook.add(order, eventWithMeta.timestamp, normalizedTickSize))
  }

  override def receiveCommand: Receive = fullCommands

  override def receiveRecover: Receive = {
    case RecoveryCompleted =>
      lastProcessedOffset match {
        case None    => log.debug("Recovery completed")
        case Some(x) => log.debug(s"Recovery completed at $x: $orderBook")
      }

      processEvents(orderBook.allOrders.map(lo => OrderAdded(lo, lo.order.timestamp)))
      updateMarketStatus(MarketStatus(lastTrade, orderBook.bestBid, orderBook.bestAsk))
      updateSnapshot(orderBook.aggregatedSnapshot)
      owner ! OrderBookRecovered(assetPair, lastSavedSnapshotOffset)

    case SnapshotOffer(_, snapshot: Snapshot) =>
      log.debug(s"Recovering from Snapshot(eventNr=${snapshot.eventNr})")
      orderBook = OrderBook(snapshot.orderBook)
      lastSavedSnapshotOffset = snapshot.eventNr
      lastProcessedOffset = lastSavedSnapshotOffset
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.warn(s"Restarting actor because of $message", reason)
    super.preRestart(reason, message)
  }

  private def saveSnapshotAt(globalEventNr: QueueEventWithMeta.Offset): Unit = {
    log.trace(s"Saving snapshot. Global seqNr=$globalEventNr, local seqNr=$lastProcessedOffset")
    saveSnapshot(Snapshot(Some(globalEventNr), orderBook.snapshot))
  }
}

object OrderBookActor {
  def props(parent: ActorRef,
            addressActor: ActorRef,
            assetPair: AssetPair,
            updateSnapshot: OrderBook.AggregatedSnapshot => Unit,
            updateMarketStatus: MarketStatus => Unit,
            settings: MatcherSettings,
            createTransaction: CreateTransaction,
            time: Time,
            normalizedTickSize: Option[Long] = None): Props =
    Props(new OrderBookActor(parent, addressActor, assetPair, updateSnapshot, updateMarketStatus, createTransaction, time, normalizedTickSize))

  def name(assetPair: AssetPair): String = assetPair.toString

  case class MarketStatus(
      lastTrade: Option[LastTrade],
      bestBid: Option[LevelAgg],
      bestAsk: Option[LevelAgg],
  )

  object MarketStatus {
    implicit val fmt: Writes[MarketStatus] = { ms =>
      Json.obj(
        "lastPrice"  -> ms.lastTrade.map(_.price),
        "lastAmount" -> ms.lastTrade.map(_.amount),
        "lastSide"   -> ms.lastTrade.map(_.side.toString),
        "bid"        -> ms.bestBid.map(_.price),
        "bidAmount"  -> ms.bestBid.map(_.amount),
        "ask"        -> ms.bestAsk.map(_.price),
        "askAmount"  -> ms.bestAsk.map(_.amount)
      )
    }
  }

  case class LastTrade(price: Long, amount: Long, side: OrderType)
  case class Snapshot(eventNr: Option[Long], orderBook: OrderBook.Snapshot)

  // Internal messages
  case class OrderBookRecovered(assetPair: AssetPair, eventNr: Option[Long])
  case class OrderBookSnapshotUpdated(assetPair: AssetPair, eventNr: Long)
}

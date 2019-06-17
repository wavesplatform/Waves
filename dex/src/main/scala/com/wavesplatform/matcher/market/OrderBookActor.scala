package com.wavesplatform.matcher.market

import akka.actor.{Actor, ActorRef, Props}
import cats.instances.option.catsStdInstancesForOption
import cats.syntax.apply._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.api._
import com.wavesplatform.matcher.market.MatcherActor.{ForceStartOrderBook, OrderBookCreated, SaveSnapshot}
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model.Events.{Event, ExchangeTransactionCreated, OrderAdded}
import com.wavesplatform.matcher.model.ExchangeTransactionCreator.CreateTransaction
import com.wavesplatform.matcher.model.OrderBook.LastTrade
import com.wavesplatform.matcher.model._
import com.wavesplatform.matcher.queue.{QueueEvent, QueueEventWithMeta}
import com.wavesplatform.matcher.settings.MatcherSettings
import com.wavesplatform.matcher.util.WorkingStash
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.utils.{LoggerFacade, ScorexLogging, Time}
import kamon.Kamon
import org.slf4j.LoggerFactory
import play.api.libs.json._

class OrderBookActor(owner: ActorRef,
                     addressActor: ActorRef,
                     snapshotStore: ActorRef,
                     assetPair: AssetPair,
                     updateSnapshot: OrderBook.AggregatedSnapshot => Unit,
                     updateMarketStatus: MarketStatus => Unit,
                     createTransaction: CreateTransaction,
                     time: Time,
                     normalizedTickSize: Option[Long] = None)
    extends Actor
    with WorkingStash
    with ScorexLogging {

  protected override lazy val log = LoggerFacade(LoggerFactory.getLogger(s"OrderBookActor[$assetPair]"))

  private var savingSnapshot          = Option.empty[QueueEventWithMeta.Offset]
  private var lastSavedSnapshotOffset = Option.empty[QueueEventWithMeta.Offset]
  private var lastProcessedOffset     = Option.empty[QueueEventWithMeta.Offset]

  private val addTimer    = Kamon.timer("matcher.orderbook.add").refine("pair" -> assetPair.toString)
  private val cancelTimer = Kamon.timer("matcher.orderbook.cancel").refine("pair" -> assetPair.toString)
  private var orderBook   = OrderBook.empty

  override def receive: Receive = recovering

  private def recovering: Receive = {
    case OrderBookSnapshotStoreActor.Response.GetSnapshot(result) =>
      result.foreach { case (_, snapshot) => orderBook = OrderBook(snapshot) }

      lastSavedSnapshotOffset = result.map(_._1)
      lastProcessedOffset = lastSavedSnapshotOffset

      log.debug(lastSavedSnapshotOffset match {
        case None    => "Recovery completed"
        case Some(x) => s"Recovery completed at $x: $orderBook"
      })

      updateMarketStatus(MarketStatus(orderBook))
      updateSnapshot(orderBook.aggregatedSnapshot)
      processEvents(orderBook.allOrders.map(lo => OrderAdded(lo, lo.order.timestamp)))

      owner ! OrderBookRecovered(assetPair, lastSavedSnapshotOffset)
      context.become(working)
      unstashAll()

    case x => stash(x)
  }

  private def working: Receive = {
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
              // We don't delete the snapshot, because it could be required after restart
              // snapshotStore ! OrderBookSnapshotStoreActor.Message.Delete(assetPair)
              context.stop(self)
          }
      }

    case MatcherActor.Ping => sender() ! MatcherActor.Pong

    case ForceStartOrderBook(p) if p == assetPair =>
      sender() ! OrderBookCreated(assetPair)

    case OrderBookSnapshotStoreActor.Response.Updated(offset) =>
      log.info(s"Snapshot has been saved at offset $offset")
      lastSavedSnapshotOffset = Some(offset)
      owner ! OrderBookSnapshotUpdateCompleted(assetPair, lastSavedSnapshotOffset)
      savingSnapshot = None

    case SaveSnapshot(globalEventNr) =>
      if (savingSnapshot.isEmpty && lastSavedSnapshotOffset.getOrElse(-1L) < globalEventNr) {
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

    updateMarketStatus(MarketStatus(orderBook))
    updateSnapshot(orderBook.aggregatedSnapshot)
  }

  private def onCancelOrder(event: QueueEventWithMeta, orderIdToCancel: ByteStr): Unit =
    cancelTimer.measure(orderBook.cancel(orderIdToCancel, event.timestamp, normalizedTickSize) match {
      case Some(cancelEvent) =>
        processEvents(List(cancelEvent))
      case None =>
        log.warn(s"Error applying $event: order not found")
    })

  private def onAddOrder(eventWithMeta: QueueEventWithMeta, order: Order): Unit = addTimer.measure {
    log.trace(s"Applied $eventWithMeta, trying to match ...")
    processEvents(orderBook.add(order, eventWithMeta.timestamp, normalizedTickSize))
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.warn(s"Restarting actor because of $message", reason)
    super.preRestart(reason, message)
  }

  private def saveSnapshotAt(globalEventNr: QueueEventWithMeta.Offset): Unit = {
    val saveSnapshot = (lastSavedSnapshotOffset, lastProcessedOffset).tupled.forall { case (saved, processed) => saved < processed }
    val toSave       = if (saveSnapshot) Some(orderBook.snapshot) else None

    if (saveSnapshot) {
      log.trace(s"About to save snapshot $orderBook")
      log.debug(
        s"Saving both offset and snapshot. Global seqNr=$globalEventNr, local seqNr=$lastProcessedOffset, current offset = $lastSavedSnapshotOffset")
    } else {
      log.debug(s"Saving offset only. Global seqNr=$globalEventNr, local seqNr=$lastProcessedOffset, current offset = $lastSavedSnapshotOffset")
    }

    snapshotStore ! OrderBookSnapshotStoreActor.Message.Update(assetPair, globalEventNr, toSave)
  }

  snapshotStore ! OrderBookSnapshotStoreActor.Message.GetSnapshot(assetPair)
}

object OrderBookActor {
  def props(parent: ActorRef,
            addressActor: ActorRef,
            snapshotStore: ActorRef,
            assetPair: AssetPair,
            updateSnapshot: OrderBook.AggregatedSnapshot => Unit,
            updateMarketStatus: MarketStatus => Unit,
            settings: MatcherSettings,
            createTransaction: CreateTransaction,
            time: Time,
            normalizedTickSize: Option[Long] = None): Props =
    Props(new OrderBookActor(parent, addressActor, snapshotStore, assetPair, updateSnapshot, updateMarketStatus, createTransaction, time, normalizedTickSize))

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

    def apply(ob: OrderBook): MarketStatus = MarketStatus(ob.getLastTrade, ob.bestBid, ob.bestAsk)
  }

  case class Snapshot(eventNr: Option[Long], orderBook: OrderBook.Snapshot)

  // Internal messages
  case class OrderBookRecovered(assetPair: AssetPair, eventNr: Option[Long])
  case class OrderBookSnapshotUpdateCompleted(assetPair: AssetPair, currentOffset: Option[Long])
}

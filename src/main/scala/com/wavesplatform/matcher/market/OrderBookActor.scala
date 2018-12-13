package com.wavesplatform.matcher.market

import akka.actor.{ActorRef, Props}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
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
import com.wavesplatform.utils.{ScorexLogging, Time}
import kamon.Kamon
import play.api.libs.json._

import scala.annotation.tailrec

class OrderBookActor(owner: ActorRef,
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

  private var lastSavedSnapshotNr: Option[Long] = Some(-1L)
  private var lastProcessedOffset: Long         = -1L

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
        request.event match {
          case x: QueueEvent.Placed   => onAddOrder(request, x.order)
          case x: QueueEvent.Canceled => onCancelOrder(request.offset, x.orderId)
          case _: QueueEvent.OrderBookDeleted =>
            sender() ! GetOrderBookResponse(OrderBookResult(time.correctedTime(), assetPair, Seq(), Seq()))
            updateSnapshot(OrderBook.empty)
            orderBook.asks.values
              .++(orderBook.bids.values)
              .flatten
              .foreach(x => context.system.eventStream.publish(Events.OrderCanceled(x, unmatchable = false)))
            context.stop(self)
        }
        lastProcessedOffset = request.offset
      }
  }

  private def snapshotsCommands: Receive = {
    case SaveSnapshotSuccess(metadata) =>
      log.info(s"Snapshot has been saved: $metadata")
      owner ! OrderBookSnapshotUpdated(assetPair, lastProcessedOffset)
      deleteSnapshots(SnapshotSelectionCriteria.Latest.copy(maxSequenceNr = metadata.sequenceNr - 1))

    case SaveSnapshotFailure(metadata, reason) =>
      lastSavedSnapshotNr = None
      log.error(s"Failed to save snapshot: $metadata", reason)

    case SaveSnapshot =>
      if (lastSavedSnapshotNr.forall(_ < lastProcessedOffset)) {
        lastSavedSnapshotNr = Some(lastProcessedOffset)
        saveSnapshot()
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
    log.trace(s"Order accepted: '${order.id()}' in '${order.assetPair.key}', trying to match ...")
    matchTimer.measure(matchOrder(eventWithMeta, LimitOrder(order)))
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
    context.system.eventStream.publish(e)
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
        createTransaction(event, eventWithMeta.timestamp) match {
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
    context.system.eventStream.publish(e)
  }

  override def receiveCommand: Receive = fullCommands

  override def receiveRecover: Receive = {
    case RecoveryCompleted =>
      updateSnapshot(orderBook)
      owner ! OrderBookSnapshotUpdated(assetPair, lastProcessedOffset)
      log.debug(s"Recovery completed: $orderBook")

    case SnapshotOffer(_, snapshot: Snapshot) =>
      orderBook = snapshot.orderBook
      lastProcessedOffset = snapshot.lastProcessedCommandNr

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

  private def saveSnapshot(): Unit = saveSnapshot(Snapshot(lastProcessedOffset, orderBook))
}

object OrderBookActor {
  def props(parent: ActorRef,
            assetPair: AssetPair,
            updateSnapshot: OrderBook => Unit,
            updateMarketStatus: MarketStatus => Unit,
            broadcastTx: ExchangeTransaction => Unit,
            settings: MatcherSettings,
            createTransaction: CreateTransaction,
            time: Time): Props =
    Props(new OrderBookActor(parent, assetPair, updateSnapshot, updateMarketStatus, broadcastTx, settings, createTransaction, time))

  def name(assetPair: AssetPair): String = assetPair.toString

  sealed trait HasAssetPair {
    def assetPair: AssetPair
  }

  case class DeleteOrderBookRequest(assetPair: AssetPair) extends HasAssetPair

  case class CancelOrder(assetPair: AssetPair, orderId: ByteStr) extends HasAssetPair

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

  case class Snapshot(lastProcessedCommandNr: Long, orderBook: OrderBook)

  // Internal messages
  case class OrderBookSnapshotUpdated(assetPair: AssetPair, lastProcessedCommandNr: Long)
}

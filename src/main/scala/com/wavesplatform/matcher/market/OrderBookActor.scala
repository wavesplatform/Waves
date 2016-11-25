package com.wavesplatform.matcher.market

import java.util.Comparator

import akka.actor.{ActorRef, Props}
import akka.persistence._
import com.wavesplatform.matcher.market.MatcherActor.{OrderAccepted, OrderRejected}
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model.{OrderValidator, _}
import com.wavesplatform.matcher.util.Cache
import com.wavesplatform.settings.WavesSettings
import scorex.crypto.encode.Base58
import scorex.transaction.SimpleTransactionModule._
import scorex.transaction.TransactionModule
import scorex.transaction.assets.exchange._
import scorex.transaction.state.database.blockchain.StoredState
import scorex.utils.ScorexLogging
import scorex.wallet.Wallet

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class OrderBookActor(assetPair: AssetPair, orderMatchedActor: ActorRef, val storedState: StoredState,
                     val wallet: Wallet, val settings: WavesSettings, val transactionModule: TransactionModule[StoredInBlock])
  extends PersistentActor
  with ScorexLogging with OrderValidator with OrderHistory with OrderMatchCreator {
  override def persistenceId: String = OrderBookActor.name(assetPair)

  private var asks = new OrderBook(assetPair, AskComparator)
  private var bids = new OrderBook(assetPair, BidComparator)

  private var restoreState = true

  context.system.scheduler.schedule(SnapshotDuration, SnapshotDuration, self, SaveSnapshot)

  override def receiveCommand: Receive = {
    case order:Order =>
      handleAddOrder(order)
    case GetOrdersRequest =>
      sender() ! GetOrdersResponse(asks.flattenOrders ++ bids.flattenOrders)
    case GetAskOrdersRequest =>
      sender() ! GetOrdersResponse(asks.flattenOrders)
    case GetBidOrdersRequest =>
      sender() ! GetOrdersResponse(bids.flattenOrders)
    case GetOrderBookRequest(pair, depth) =>
      getOrderBook(pair, depth)
    case SaveSnapshot =>
        deleteSnapshots(SnapshotSelectionCriteria.Latest)
        saveSnapshot(Snapshot(asks, bids, ordersRemainingAmount))
    case SaveSnapshotSuccess(metadata) =>
      log.info(s"Snapshot saved with metadata $metadata")
    case SaveSnapshotFailure(metadata, reason) =>
      log.error(s"Failed to save snapshot: $metadata, $reason.")
    case GetOrderStatus(id) =>
      handleOrderStatus(id)
  }

  def handleOrderStatus(id: String): Unit = {
    sender() ! GetOrderStatusResponse(getOrderStatus(id))
  }


  def getOrderBook(pair: AssetPair, depth: Int): Unit = {
    if (pair == assetPair) {
      val d = Math.min(depth, MaxDepth)
      sender() ! GetOrderBookResponse(bids.take(d), asks.take(d))
    } else sender() ! GetOrderBookResponse(Seq(), Seq())
  }

  override def receiveRecover: Receive = {
    case evt: OrderEvent => log.info("Event: {}", evt); applyEvent(evt)
    case RecoveryCompleted => log.info("Recovery completed!"); restoreState = false
    case SnapshotOffer(metadata, snapshot: Snapshot) =>
      log.info(s"Recovering OrderBook from snapshot: $snapshot for $persistenceId")
      asks = snapshot.asks
      bids = snapshot.bids
  }

  def handleAddOrder(order: Order): Unit = {
    val v = validateNewOrder(order)
    if (v) {
      persistAsync(OrderAdded(OrderItem(order))) { evt =>
        sender() ! OrderAccepted(order)
        place(evt.order)
      }
    } else {
      sender() ! OrderRejected(v.messages)
    }
  }

  private def applyEvent(orderEvent: OrderEvent) = orderEvent match {
    case OrderAdded(order) => place(order)
  }

  private def putOrder(order: OrderItem): Unit = {
    order.order.orderType match {
      case OrderType.BUY => bids.add(order)
      case OrderType.SELL => asks.add(order)
    }
  }

  @tailrec
  private def runMatching(order: OrderItem): (Seq[OrderMatch], Long) = {
    val (_, oppositeQ) = order.order.orderType match {
      case OrderType.BUY => (bids, asks)
      case OrderType.SELL => (asks, bids)
    }

    val (executedOrders, remaining) = oppositeQ.runMatching(order)

    val txs = executedOrders.map(createTransaction(order.order, _))
    val invalid = txs.filterNot(p => isValid(p._1))

    if (invalid.nonEmpty) {
      val invalidItems = invalid.map(_._2)
      oppositeQ.removeMatched(invalidItems)
      removeOrderItems(invalidItems)
      runMatching(order)
    }
    else {
      oppositeQ.removeMatched(executedOrders)
      removeOrderItems(executedOrders)
      (txs.map(_._1), remaining)
    }
  }

  private def place(order: OrderItem) {
    val (omTxs, remaining) = runMatching(order)
    if (omTxs.nonEmpty && !restoreState) {
      log.info(s"$order executed")
      sendToNetwork(omTxs)
    }

    if (remaining > 0) {
      val remOrder = order.copy(amount = remaining)
      putOrder(remOrder)
      addOpenOrder(remOrder)
    }
  }

}

object OrderBookActor {
  def props(assetPair: AssetPair, orderMatchedActor: ActorRef, storedState: StoredState,
            wallet: Wallet, settings: WavesSettings, transactionModule: TransactionModule[StoredInBlock]): Props =
    Props(new OrderBookActor(assetPair, orderMatchedActor, storedState, wallet, settings, transactionModule))
  def name(assetPair: AssetPair): String = assetPair.first.map(Base58.encode).getOrElse("WAVES") + "-" +
    assetPair.second.map(Base58.encode).getOrElse("WAVES")

  val MaxDepth = 50
  val SnapshotDuration = 1.minutes

  //protocol
  case object GetOrdersRequest
  case class GetOrderBookRequest(pair: AssetPair, depth: Int = MaxDepth)
  case object GetBidOrdersRequest
  case object GetAskOrdersRequest
  case class GetOrdersResponse(orders: Seq[OrderItem])
  case class GetOrderBookResponse(bids: Seq[LevelAgg], asks: Seq[LevelAgg])
  case object SaveSnapshot
  case class GetOrderStatus(id: String)
  case class GetOrderStatusResponse(status: OrderItem.OrderStatus)


  // events
  sealed trait OrderEvent

  @SerialVersionUID(-5350485695558994597L)
  case class Snapshot(asks: OrderBook, bids: OrderBook, history: Cache[String, (Long, Long)])

  @SerialVersionUID(-3697114578758882607L)
  case class OrderAdded(order: OrderItem) extends OrderEvent
  case class OrderMatched(order: Order, items: Seq[OrderItem]) extends OrderEvent
}

case object BidComparator extends Comparator[Long] {
  def compare(o1: Long, o2: Long) = -o1.compareTo(o2)
}

case object AskComparator extends Comparator[Long] {
  def compare(o1: Long, o2: Long) = o1.compareTo(o2)
}


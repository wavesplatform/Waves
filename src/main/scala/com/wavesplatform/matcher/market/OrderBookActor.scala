package com.wavesplatform.matcher.market

import akka.actor.Props
import akka.persistence._
import com.wavesplatform.matcher.market.MatcherActor.{OrderAccepted, OrderRejected}
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model.Events.{Event, OrderAdded, OrderExecuted}
import com.wavesplatform.matcher.model.MatcherModel._
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

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class OrderBookActor(assetPair: AssetPair, val storedState: StoredState,
                     val wallet: Wallet, val settings: WavesSettings,
                     val transactionModule: TransactionModule[StoredInBlock])
  extends PersistentActor
  with ScorexLogging with OrderValidator with OrderHistory with OrderMatchCreator {
  override def persistenceId: String = OrderBookActor.name(assetPair)

  private var orderBook = OrderBook.empty
  private var restoreState = true

  context.system.scheduler.schedule(SnapshotDuration, SnapshotDuration, self, SaveSnapshot)

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
      getOrderBook(pair, depth)
    case SaveSnapshot =>
        deleteSnapshots(SnapshotSelectionCriteria.Latest)
        saveSnapshot(Snapshot(orderBook, ordersRemainingAmount))
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
    def aggregateLevel(l: (Price, Level[LimitOrder])) = LevelAgg(l._1, l._2.foldLeft(0L)((b, o) => b + o.amount))

    if (pair == assetPair) {
      val d = Math.min(depth, MaxDepth)
      sender() ! GetOrderBookResponse(orderBook.bids.take(d).map(aggregateLevel).toSeq,
        orderBook.asks.take(d).map(aggregateLevel).toSeq)
    } else sender() ! GetOrderBookResponse(Seq(), Seq())
  }

  override def receiveRecover: Receive = {
    case evt: Event => log.debug("Event: {}", evt); applyEvent(evt)
    case RecoveryCompleted => log.info("Recovery completed!");
    case SnapshotOffer(metadata, snapshot: Snapshot) =>
      log.info(s"Recovering OrderBook from snapshot: $snapshot for $persistenceId")
      orderBook = snapshot.orderBook
      ordersRemainingAmount = snapshot.history
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
      case OrderAdded(o) => addOpenOrder(o)
      case OrderExecuted(s, c) =>
      case _ =>
    }
  }

  def matchOrder(limitOrder: LimitOrder): Unit = {
    persist(OrderBook.matchOrder(orderBook, limitOrder)) { e =>
      handleEvent(e).foreach(matchOrder)
    }
  }

  def handleEvent(e: Event): Option[LimitOrder] = {
    applyEvent(e)
    context.system.eventStream.publish(e)

    e match {
      case e: OrderAdded =>
        None
      case e@OrderExecuted(o, c) =>
        val tx = createTransaction(o.order, c)
        if (isValid(tx)) {
          sendToNetwork(tx)
        }
        if (e.submittedRemaining > 0) Some(o.partial(e.submittedRemaining))
        else None
      case _ => None
    }
  }

}

object OrderBookActor {
  def props(assetPair: AssetPair, storedState: StoredState,
            wallet: Wallet, settings: WavesSettings, transactionModule: TransactionModule[StoredInBlock]): Props =
    Props(new OrderBookActor(assetPair, storedState, wallet, settings, transactionModule))
  def name(assetPair: AssetPair): String = assetPair.first.map(Base58.encode).getOrElse("WAVES") + "-" +
    assetPair.second.map(Base58.encode).getOrElse("WAVES")

  val MaxDepth = 50
  val SnapshotDuration = 1.minutes

  //protocol
  case object GetOrdersRequest
  case class GetOrderBookRequest(pair: AssetPair, depth: Int = MaxDepth)
  case object GetBidOrdersRequest
  case object GetAskOrdersRequest
  case class GetOrdersResponse(orders: Seq[LimitOrder])
  case class GetOrderBookResponse(bids: Seq[LevelAgg], asks: Seq[LevelAgg])
  case object SaveSnapshot
  case class GetOrderStatus(id: String)
  case class GetOrderStatusResponse(status: LimitOrder.OrderStatus)


  @SerialVersionUID(-5350485695558994597L)
  case class Snapshot(orderBook: OrderBook, history: Cache[String, (Long, Long)])

  val bidsOrdering: Ordering[Long] = new Ordering[Long] {
    def compare(x: Long, y: Long): Int = - Ordering.Long.compare(x, y)
  }

  val asksOrdering: Ordering[Long] = new Ordering[Long] {
    def compare(x: Long, y: Long): Int = Ordering.Long.compare(x, y)
  }
}


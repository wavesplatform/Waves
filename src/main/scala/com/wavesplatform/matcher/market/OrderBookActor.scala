package com.wavesplatform.matcher.market

import java.util.Comparator

import akka.actor.{ActorRef, Props}
import akka.persistence.{PersistentActor, RecoveryCompleted}
import com.wavesplatform.matcher.api.OrderService
import com.wavesplatform.matcher.market.MatcherActor.{OrderAccepted, OrderRejected}
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model.{LevelAgg, OrderBook, OrderItem}
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType, Validation}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.utils.ScorexLogging

import scala.annotation.tailrec

object OrderBookActor {
  def props(assetPair: AssetPair, orderMatchedActor: ActorRef, storedState: StoredState): Props =
    Props(new OrderBookActor(assetPair, orderMatchedActor, storedState))
  def name(assetPair: AssetPair): String = assetPair.first.map(Base58.encode).getOrElse("WAVES") + "-" +
    assetPair.second.map(Base58.encode).getOrElse("WAVES")

  //protocol
  val MaxDepth = 50
  case object GetOrdersRequest
  case class GetOrderBookRequest(pair: AssetPair, depth: Int = MaxDepth)
  case object GetBidOrdersRequest
  case object GetAskOrdersRequest
  case class GetOrdersResponse(orders: Seq[OrderItem])
  case class GetOrderBookResponse(bids: Seq[LevelAgg], asks: Seq[LevelAgg])

  // events
  sealed trait OrderEvent

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

class OrderBookActor(assetPair: AssetPair, orderMatchedActor: ActorRef, val storedState: StoredState)
  extends PersistentActor
  with ScorexLogging with OrderService {
  override def persistenceId: String = OrderBookActor.name(assetPair)

  private val asks = new OrderBook(assetPair, AskComparator)
  private val bids = new OrderBook(assetPair, BidComparator)

  private var restoreState = true

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
  }

  def handleAddOrder(order: Order): Unit = {
    persistAsync(OrderAdded(OrderItem(order))) { evt =>
      val v = validateNewOrder(order)
      if (v) {
        sender() ! OrderAccepted(order)
        place(evt.order)
      } else {
        sender() ! OrderRejected(v.messages)
      }
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
  private def doMatching(order: OrderItem): (Seq[OrderItem], Long) = {
    val (_, oppositeQ) = order.order.orderType match {
      case OrderType.BUY => (bids, asks)
      case OrderType.SELL => (asks, bids)
    }

    val (executedOrders, remaining) = oppositeQ.doMatching(order)

    val inValid = executedOrders.filterNot(validateOrderItem)
    if (inValid.nonEmpty) {
      oppositeQ.removeMatched(inValid)
      removeOpenOrders(inValid)
      doMatching(order)
    }
    else {
      oppositeQ.removeMatched(executedOrders)
      removeOpenOrders(executedOrders)
      (executedOrders, remaining)
    }
  }

  private def place(order: OrderItem) {
    val (executedOrders, remaining) = doMatching(order)
    if (executedOrders.nonEmpty && !restoreState) {
      log.info(s"${order.order.orderType} executed: {}", executedOrders)
      orderMatchedActor ! OrderMatched(order.order, executedOrders)
    }

    if (remaining > 0) {
      val remOrder = order.copy(amount = remaining)
      putOrder(remOrder)
      addOpenOrder(remOrder)
    }
  }

}

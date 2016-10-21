package com.wavesplatform.matcher.market

import java.util.Comparator

import akka.actor.Props
import akka.persistence.{PersistentActor, RecoveryCompleted}

import com.wavesplatform.matcher.market.MatcherActor.OrderAccepted
import com.wavesplatform.matcher.market.OrderBookActor._
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}
import scorex.utils.ScorexLogging

object OrderBookActor {
  def props(assetPair: AssetPair): Props = Props(new OrderBookActor(assetPair))
  def name(assetPair: AssetPair): String = s"${assetPair.first}-${assetPair.second}"

  //protocol
  case object GetOrdersRequest
  case object GetBidOrdersRequest
  case object GetAskOrdersRequest
  case class GetOrdersResponse(orders: Seq[Order])

  // events
  sealed trait OrderEvent
  case class OrderAdded(order: Order) extends OrderEvent
  case class OrderMatched(orders: Seq[Order]) extends OrderEvent
}

case object BidComparator extends Comparator[Long] {
  def compare(o1: Long, o2: Long) = -o1.compareTo(o2)
}

case object AskComparator extends Comparator[Long] {
  def compare(o1: Long, o2: Long) = o1.compareTo(o2)
}

class OrderBookActor(assetPair: AssetPair) extends PersistentActor with ScorexLogging {
  override def persistenceId: String = assetPair.toString()

  private val asks = new OrderBook(assetPair, AskComparator)
  private val bids = new OrderBook(assetPair, BidComparator)

  override def receiveCommand: Receive = {
    case order:Order =>
      handleAddOrder(order)
    case GetOrdersRequest =>
      sender() ! GetOrdersResponse(asks.flattenOrders ++ bids.flattenOrders)
    case GetAskOrdersRequest =>
      sender() ! GetOrdersResponse(asks.flattenOrders)
    case GetBidOrdersRequest =>
      sender() ! GetOrdersResponse(bids.flattenOrders)
  }

  override def receiveRecover: Receive = {
    case evt: OrderEvent => log.info("Event: {}", evt); applyEvent(evt)
    case RecoveryCompleted => log.info("Recovery completed!")
  }

  def handleAddOrder(order: Order): Unit = {
    persistAsync(OrderAdded(order)) { evt =>
      place(order)
      sender() ! OrderAccepted(order)
    }
  }

  private def applyEvent(orderEvent: OrderEvent) = orderEvent match {
    case OrderAdded(order) => place(order)
    case OrderMatched(orders) =>
  }

  private def putOrder(order: Order): Unit = {
    order.orderType match {
      case OrderType.BUY => bids.add(order)
      case OrderType.SELL => asks.add(order)
    }
  }

  private def place(order: Order) {
    val (executedOrders, remaining) = order.orderType match {
      case OrderType.BUY => asks.execute(order)
      case OrderType.SELL => bids.execute(order)
    }

    if (executedOrders.nonEmpty) {
      log.info(s"${order.orderType} executed: {}", executedOrders)
      context.system.eventStream.publish(OrderMatched(executedOrders))
    }

    if (remaining > 0) {
      putOrder(order.copy(amount = remaining))
    }
  }

}

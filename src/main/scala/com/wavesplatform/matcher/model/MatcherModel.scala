package com.wavesplatform.matcher.model

import com.wavesplatform.matcher.model.MatcherModel.{OrderId, Price}
import scorex.transaction.assets.exchange.{Order, OrderType}

object MatcherModel {
  type Price = Long
  type Level[+A] = Vector[A]
  type OrderId = String
}

case class LevelAgg(price: Long, amount: Long)

sealed trait LimitOrder {
  def price: Price
  def amount: Long
  def order: Order
  def partial(amount: Long): LimitOrder

  def sellAmount: Long
  def buyAmount: Long
  def feeAmount: Long = (BigInt(amount) * order.matcherFee  / order.amount).toLong
}

case class BuyLimitOrder(price: Price, amount: Long, order: Order) extends LimitOrder {
  def partial(amount: Price): LimitOrder = copy(amount = amount)
  def buyAmount: Long = amount
  def sellAmount: Long = (BigInt(amount) * Order.PriceConstant / price).longValue()
}
case class SellLimitOrder(price: Price, amount: Long, order: Order) extends LimitOrder {
  def partial(amount: Price): LimitOrder = copy(amount = amount)
  def sellAmount: Long = amount
  def buyAmount: Long = (BigInt(amount) * Order.PriceConstant / price).longValue()
}


object LimitOrder {
  sealed trait OrderStatus
  case object Accepted extends OrderStatus
  case object NotFound extends OrderStatus
  case class PartiallyFilled(remainingAmount: Long) extends OrderStatus
  case object Filled extends OrderStatus

  def apply(o: Order): LimitOrder = o.orderType match {
    case OrderType.BUY => BuyLimitOrder(o.price, o.amount, o).copy()
    case OrderType.SELL => SellLimitOrder(o.price, o.amount, o)
  }

}

object Commands {
  sealed trait Command
  case class AddLimitOrder(o: LimitOrder) extends Command
  case class CancelOrder(id: OrderId) extends Command
}

object Events {
  sealed trait Event
  case class OrderExecuted(submittedOrder: LimitOrder, counterOrder: LimitOrder) extends Event {
    def counterRemaining: Long = Math.max(counterOrder.amount - submittedOrder.amount, 0)
    def submittedRemaining: Long = Math.max(submittedOrder.amount - counterOrder.amount, 0)
  }
  @SerialVersionUID(-3697114578758882607L)
  case class OrderAdded(order: LimitOrder) extends Event
  case class OrderRejected(id: OrderId) extends Event
  case class OrderCanceled(id: OrderId) extends Event
}

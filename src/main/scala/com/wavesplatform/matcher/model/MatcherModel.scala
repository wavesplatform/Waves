package com.wavesplatform.matcher.model

import com.wavesplatform.matcher.model.MatcherModel.Price
import play.api.libs.json.{JsValue, Json}
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

  def getSpendAmount: Long
  def getReceiveAmount: Long
  def feeAmount: Long = (BigInt(amount) * order.matcherFee  / order.amount).toLong
}

case class BuyLimitOrder(price: Price, amount: Long, order: Order) extends LimitOrder {
  def partial(amount: Price): LimitOrder = copy(amount = amount)
  def getReceiveAmount: Long = (BigInt(amount) * Order.PriceConstant / price).longValue()
  def getSpendAmount: Long = amount
}
case class SellLimitOrder(price: Price, amount: Long, order: Order) extends LimitOrder {
  def partial(amount: Price): LimitOrder = copy(amount = amount)
  def getSpendAmount: Long = (BigInt(amount) * Order.PriceConstant / price).longValue()
  def getReceiveAmount: Long = amount
}


object LimitOrder {
  sealed trait OrderStatus {
    def json: JsValue
  }
  case object Accepted extends OrderStatus {
    def json = Json.obj("status" -> "Accepted")
  }
  case object NotFound extends OrderStatus {
    def json = Json.obj("status" -> "NotFound")
  }
  case class PartiallyFilled(filled: Long) extends OrderStatus {
    def json = Json.obj("status" -> "PartiallyFilled", "filledAmount" -> filled)
  }
  case object Filled extends OrderStatus {
    def json = Json.obj("status" -> "Filled")
  }
  case class Cancelled(filled: Long) extends OrderStatus {
    def json = Json.obj("status" -> "Cancelled", "filledAmount" -> filled)
  }

  def apply(o: Order): LimitOrder = o.orderType match {
    case OrderType.BUY => BuyLimitOrder(o.price, o.amount, o).copy()
    case OrderType.SELL => SellLimitOrder(o.price, o.amount, o)
  }

  def limitOrder(price: Long, amount: Long, o: Order): LimitOrder = o.orderType match {
    case OrderType.BUY => BuyLimitOrder(price, amount, o).copy()
    case OrderType.SELL => SellLimitOrder(price, amount, o)
  }

}

object Events {
  sealed trait Event extends Serializable
  @SerialVersionUID(-6952325887070115993L)
  case class OrderExecuted(submitted: LimitOrder, counter: LimitOrder) extends Event {
    def counterRemaining: Long = math.max(counter.amount - submitted.amount, 0)
    def submittedRemaining: Long = math.max(submitted.amount - counter.amount, 0)
    def executedAmount: Long = math.min(submitted.amount, counter.amount)
    def submittedExecuted = submitted.partial(amount = executedAmount)
    def counterExecuted = counter.partial(amount = executedAmount)
    def isCounterFilled: Boolean = counterRemaining == 0L

  }
  @SerialVersionUID(-3697114578758882607L)
  case class OrderAdded(order: LimitOrder) extends Event
  @SerialVersionUID(-2668400548412162900L)
  case class OrderCanceled(limitOrder: LimitOrder) extends Event
}

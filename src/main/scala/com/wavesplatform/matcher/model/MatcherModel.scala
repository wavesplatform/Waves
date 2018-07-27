package com.wavesplatform.matcher.model

import cats.Monoid
import cats.implicits._
import com.wavesplatform.matcher.model.MatcherModel.Price
import com.wavesplatform.state.{ByteStr, Portfolio}
import play.api.libs.json.{JsObject, JsValue, Json}
import scorex.account.Address
import scorex.transaction.assets.exchange._
import scorex.transaction.{AssetAcc, AssetId}

import scala.util.Try

object MatcherModel {
  type Price     = Long
  type Level[+A] = Vector[A]
  type OrderId   = String
}

case class LevelAgg(price: Long, amount: Long)

sealed trait LimitOrder {
  def price: Price
  def amount: Long
  def order: Order
  def partial(amount: Long): LimitOrder

  def getSpendAmount: Long
  def getReceiveAmount: Long
  def feeAmount: Long       = Try((BigInt(amount) * order.matcherFee / order.amount).bigInteger.longValueExact()).getOrElse(Long.MaxValue)
  def remainingAmount: Long = order.amount - amount
  val remainingFee: Long = order.matcherFee - Try((BigInt(remainingAmount) * order.matcherFee / order.amount).bigInteger.longValueExact())
    .getOrElse(0L)

  def spentAcc: AssetAcc = AssetAcc(order.senderPublicKey, order.getSpendAssetId)
  def rcvAcc: AssetAcc   = AssetAcc(order.senderPublicKey, order.getReceiveAssetId)
  def feeAcc: AssetAcc   = AssetAcc(order.senderPublicKey, None)

  def spentAsset: Option[ByteStr] = order.getSpendAssetId
  def rcvAsset: Option[ByteStr]   = order.getReceiveAssetId
  def feeAsset: Option[ByteStr]   = None
}

case class BuyLimitOrder(price: Price, amount: Long, order: Order) extends LimitOrder {
  def partial(amount: Price): LimitOrder = copy(amount = amount)
  def getReceiveAmount: Long             = amount
  def getSpendAmount: Long               = Try((BigInt(amount) * price / Order.PriceConstant).bigInteger.longValueExact()).getOrElse(Long.MaxValue)
}
case class SellLimitOrder(price: Price, amount: Long, order: Order) extends LimitOrder {
  def partial(amount: Price): LimitOrder = copy(amount = amount)
  def getSpendAmount: Long               = amount
  def getReceiveAmount: Long             = Try((BigInt(amount) * price / Order.PriceConstant).bigInteger.longValueExact()).getOrElse(Long.MaxValue)
}

object LimitOrder {
  sealed trait OrderStatus {
    def name: String
    def json: JsValue
    def isFinal: Boolean
    def ordering: Int
  }

  case object Accepted extends OrderStatus {
    val name             = "Accepted"
    def json: JsObject   = Json.obj("status" -> name)
    val isFinal: Boolean = false
    val ordering         = 1
  }
  case object NotFound extends OrderStatus {
    val name             = "NotFound"
    def json: JsObject   = Json.obj("status" -> name)
    val isFinal: Boolean = true
    val ordering         = 5
  }
  case class PartiallyFilled(filled: Long) extends OrderStatus {
    val name             = "PartiallyFilled"
    def json: JsObject   = Json.obj("status" -> name, "filledAmount" -> filled)
    val isFinal: Boolean = false
    val ordering         = 1
  }
  case object Filled extends OrderStatus {
    val name             = "Filled"
    def json             = Json.obj("status" -> name)
    val isFinal: Boolean = true
    val ordering         = 3
  }
  case class Cancelled(filled: Long) extends OrderStatus {
    val name             = "Cancelled"
    def json             = Json.obj("status" -> name, "filledAmount" -> filled)
    val isFinal: Boolean = true
    val ordering         = 3
  }

  def apply(o: Order): LimitOrder = o.orderType match {
    case OrderType.BUY  => BuyLimitOrder(o.price, o.amount, o).copy()
    case OrderType.SELL => SellLimitOrder(o.price, o.amount, o)
  }

  def limitOrder(price: Long, amount: Long, o: Order): LimitOrder = o.orderType match {
    case OrderType.BUY  => BuyLimitOrder(price, amount, o).copy()
    case OrderType.SELL => SellLimitOrder(price, amount, o)
  }

  def validateAmount(lo: LimitOrder): Validation = lo.order.isValidAmount(lo.price, lo.amount)
}

object Events {
  sealed trait Event
  case class OrderExecuted(submitted: LimitOrder, counter: LimitOrder) extends Event {
    def counterRemaining: Long              = math.max(counter.amount - submitted.amount, 0)
    def counterRemainingOrder: LimitOrder   = counter.partial(counterRemaining)
    def submittedRemaining: Long            = math.max(submitted.amount - counter.amount, 0)
    def submittedRemainingOrder: LimitOrder = submitted.partial(submittedRemaining)
    def executedAmount: Long                = math.min(submitted.amount, counter.amount)
    def submittedExecuted                   = submitted.partial(amount = executedAmount)
    def counterExecuted                     = counter.partial(amount = executedAmount)
    def isCounterFilled: Boolean            = counterRemaining == 0L

  }
  case class OrderAdded(order: LimitOrder)         extends Event
  case class OrderCanceled(limitOrder: LimitOrder) extends Event

  case class ExchangeTransactionCreated(tx: ExchangeTransaction)

  case class BalanceChanged(changes: Map[Address, BalanceChanged.Changes]) {
    def isEmpty: Boolean = changes.isEmpty
  }
  object BalanceChanged {
    val empty: BalanceChanged = BalanceChanged(Map.empty)
    case class Changes(updatedPortfolio: Portfolio, changedAssets: Set[Option[AssetId]])
  }

  def createOrderInfo(event: Event): Map[ByteStr, (Order, OrderInfo)] = {
    event match {
      case OrderAdded(lo) =>
        Map(lo.order.id() -> (lo.order -> OrderInfo(lo.order.amount, 0L, false)))
      case oe: OrderExecuted =>
        val (o1, o2) = (oe.submittedExecuted, oe.counterExecuted)
        Map(
          o1.order.id() -> (o1.order -> OrderInfo(o1.order.amount, o1.amount, false)),
          o2.order.id() -> (o2.order -> OrderInfo(o2.order.amount, o2.amount, false))
        )
      case OrderCanceled(lo) =>
        Map(lo.order.id() -> (lo.order -> OrderInfo(lo.order.amount, 0, true)))
    }
  }

  def createOpenPortfolio(event: Event): Map[Address, OpenPortfolio] = {
    def overdraftFee(lo: LimitOrder): Long = {
      if (lo.feeAcc == lo.rcvAcc) math.max(lo.feeAmount - lo.getReceiveAmount, 0L) else lo.feeAmount
    }

    event match {
      case OrderAdded(lo) =>
        Map(
          lo.order.senderPublicKey.toAddress -> OpenPortfolio(
            Monoid.combine(
              Map(lo.spentAsset -> lo.getSpendAmount),
              Map(lo.feeAsset   -> overdraftFee(lo))
            )))
      case oe: OrderExecuted =>
        val (o1, o2) = (oe.submittedExecuted, oe.counterExecuted)
        val op1 = OpenPortfolio(
          Monoid.combine(
            Map(o1.spentAsset -> -o1.getSpendAmount),
            Map(o1.feeAsset   -> -overdraftFee(o1))
          ))
        val op2 = OpenPortfolio(
          Monoid.combine(
            Map(o2.spentAsset -> -o2.getSpendAmount),
            Map(o2.feeAsset   -> -overdraftFee(o2))
          ))
        Monoid.combine(
          Map(o1.order.senderPublicKey.toAddress -> op1),
          Map(o2.order.senderPublicKey.toAddress -> op2)
        )
      case OrderCanceled(lo) =>
        val feeDiff = if (lo.feeAcc == lo.rcvAcc) math.max(lo.remainingFee - lo.getReceiveAmount, 0L) else lo.remainingFee
        Map(
          lo.order.senderPublicKey.toAddress ->
            OpenPortfolio(
              Monoid.combine(
                Map(lo.spentAsset -> -lo.getSpendAmount),
                Map(lo.feeAsset   -> -feeDiff)
              )))
    }
  }
}

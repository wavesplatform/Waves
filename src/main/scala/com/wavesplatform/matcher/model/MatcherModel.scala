package com.wavesplatform.matcher.model

import cats.Monoid
import cats.implicits._
import com.wavesplatform.matcher.model.MatcherModel.Price
import com.wavesplatform.state.{ByteStr, Portfolio}
import play.api.libs.json.{JsObject, JsValue, Json}
import scorex.account.Address
import scorex.transaction.assets.exchange._
import scorex.transaction.{AssetAcc, AssetId}

import scala.math.BigDecimal.RoundingMode

object MatcherModel {
  type Price     = Long
  type Level[+A] = Vector[A]
  type OrderId   = String
}

case class LevelAgg(price: Long, amount: Long)

sealed trait LimitOrder {
  def price: Price
  def amount: Long
  def fee: Long
  def order: Order
  def partial(amount: Long, fee: Long): LimitOrder

  def getSpendAmount: Long
  def getReceiveAmount: Long

  def spentAcc: AssetAcc = AssetAcc(order.senderPublicKey, order.getSpendAssetId)
  def rcvAcc: AssetAcc   = AssetAcc(order.senderPublicKey, order.getReceiveAssetId)
  def feeAcc: AssetAcc   = AssetAcc(order.senderPublicKey, None)

  def spentAsset: Option[ByteStr] = order.getSpendAssetId
  def rcvAsset: Option[ByteStr]   = order.getReceiveAssetId
  def feeAsset: Option[ByteStr]   = None

  def minAmountOfAmountAsset: Long         = minimalAmountOfAmountAssetByPrice(price)
  def amountOfPriceAsset: Long             = (BigDecimal(amount) * price / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
  def amountOfAmountAsset: Long            = correctedAmountOfAmountAsset(price, amount)
  def executionAmount(o: LimitOrder): Long = correctedAmountOfAmountAsset(o.price, amount)

  def isValid: Boolean =
    amount > 0 && amount >= minAmountOfAmountAsset && amount < Order.MaxAmount && getSpendAmount > 0 && getReceiveAmount > 0

  protected def minimalAmountOfAmountAssetByPrice(p: Long): Long = (BigDecimal(Order.PriceConstant) / p).setScale(0, RoundingMode.CEILING).toLong
  protected def correctedAmountOfAmountAsset(p: Long, a: Long): Long = {
    val settledTotal = (BigDecimal(p) * a / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
    (BigDecimal(settledTotal) / p * Order.PriceConstant).setScale(0, RoundingMode.CEILING).toLong
  }
}

case class BuyLimitOrder(price: Price, amount: Long, fee: Long, order: Order) extends LimitOrder {
  def partial(amount: Long, fee: Long): LimitOrder = copy(amount = amount, fee = fee)
  def getReceiveAmount: Long                       = amountOfAmountAsset
  def getSpendAmount: Long                         = amountOfPriceAsset
}

case class SellLimitOrder(price: Price, amount: Long, fee: Long, order: Order) extends LimitOrder {
  def partial(amount: Long, fee: Long): LimitOrder = copy(amount = amount, fee = fee)
  def getReceiveAmount: Long                       = amountOfPriceAsset
  def getSpendAmount: Long                         = amountOfAmountAsset
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
  case class Filled(filled: Long) extends OrderStatus {
    val name             = "Filled"
    def json: JsObject   = Json.obj("status" -> name, "filledAmount" -> filled)
    val isFinal: Boolean = true
    val ordering         = 3
  }
  case class Cancelled(filled: Long) extends OrderStatus {
    val name             = "Cancelled"
    def json: JsObject   = Json.obj("status" -> name, "filledAmount" -> filled)
    val isFinal: Boolean = true
    val ordering         = 3
  }

  def apply(o: Order): LimitOrder = {
    val partialFee = getPartialFee(o.matcherFee, o.amount, o.amount)
    o.orderType match {
      case OrderType.BUY  => BuyLimitOrder(o.price, o.amount, partialFee, o)
      case OrderType.SELL => SellLimitOrder(o.price, o.amount, partialFee, o)
    }
  }

  def limitOrder(price: Long, remainingAmount: Long, remainingFee: Long, o: Order): LimitOrder = {
    o.orderType match {
      case OrderType.BUY  => BuyLimitOrder(price, remainingAmount, remainingFee, o)
      case OrderType.SELL => SellLimitOrder(price, remainingAmount, remainingFee, o)
    }
  }

  def getPartialFee(matcherFee: Long, totalAmount: Long, partialAmount: Long): Long = {
    // Should not round! It could lead to forks. See ExchangeTransactionDiff
    (BigInt(matcherFee) * partialAmount / totalAmount).toLong
  }
}

object Events {

  sealed trait Event

  case class OrderExecuted(submitted: LimitOrder, counter: LimitOrder) extends Event {
    def executedAmount: Long = math.min(submitted.executionAmount(counter), counter.amountOfAmountAsset)

    def counterRemainingAmount: Long = math.max(counter.amount - executedAmount, 0)
    def counterExecutedFee: Long     = LimitOrder.getPartialFee(counter.order.matcherFee, counter.order.amount, executedAmount)
    def counterRemainingFee: Long    = math.max(counter.fee - counterExecutedFee, 0)
    def counterExecuted: LimitOrder  = counter.partial(amount = executedAmount, fee = counterExecutedFee)

    def submittedRemainingAmount: Long = math.max(submitted.amount - executedAmount, 0)
    def submittedExecutedFee: Long     = LimitOrder.getPartialFee(submitted.order.matcherFee, submitted.order.amount, executedAmount)
    def submittedRemainingFee: Long    = math.max(submitted.fee - submittedExecutedFee, 0)
    def submittedExecuted: LimitOrder  = submitted.partial(amount = executedAmount, fee = submittedExecutedFee)
  }

  case class OrderAdded(order: LimitOrder) extends Event

  case class OrderCanceled(limitOrder: LimitOrder, unmatchable: Boolean) extends Event

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
        Map((lo.order.id(), (lo.order, OrderInfo(lo.order.amount, 0L, canceled = false, Some(lo.minAmountOfAmountAsset), lo.fee))))
      case oe: OrderExecuted =>
        val (o1, o2) = (oe.submittedExecuted, oe.counterExecuted)
        Map(
          (o1.order.id(), (o1.order, OrderInfo(o1.order.amount, o1.amount, canceled = false, Some(o1.minAmountOfAmountAsset), o1.fee))),
          (o2.order.id(), (o2.order, OrderInfo(o2.order.amount, o2.amount, canceled = false, Some(o2.minAmountOfAmountAsset), o2.fee)))
        )
      case OrderCanceled(lo, unmatchable) =>
        Map((lo.order.id(), (lo.order, OrderInfo(lo.order.amount, 0L, canceled = !unmatchable, Some(lo.minAmountOfAmountAsset), lo.fee))))
    }
  }

  def createOpenPortfolio(event: Event): Map[Address, OpenPortfolio] = {
    def overdraftFee(lo: LimitOrder): Long = {
      val feeAmount = LimitOrder.getPartialFee(lo.order.matcherFee, lo.order.amount, lo.amount)
      if (lo.feeAcc == lo.rcvAcc) math.max(feeAmount - lo.getReceiveAmount, 0L) else feeAmount
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
      case OrderCanceled(lo, unmatchable) =>
        val feeDiff = if (unmatchable) 0 else if (lo.feeAcc == lo.rcvAcc) math.max(lo.fee - lo.getReceiveAmount, 0L) else lo.fee
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

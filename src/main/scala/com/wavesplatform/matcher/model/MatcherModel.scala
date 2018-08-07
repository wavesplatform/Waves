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
import scala.util.Try

object MatcherModel {
  type Price     = Long
  type Level[+A] = Vector[A]
  type OrderId   = String
}

case class LevelAgg(price: Long, amount: Long)

sealed trait LimitOrder {
  def price: Price
  def amount: Long // remaining
  def fee: Long    // remaining
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
  def amountOfPriceAsset: Long             = longExact(BigInt(amount) * price / Order.PriceConstant, Long.MaxValue)
  def amountOfAmountAsset: Long            = correctedAmountOfAmountAsset(minAmountOfAmountAsset, amount)
  def executionAmount(o: LimitOrder): Long = correctedAmountOfAmountAsset(minimalAmountOfAmountAssetByPrice(o.price), amount)

  def isValid: Boolean =
    amount > 0 && amount >= minAmountOfAmountAsset && amount < Order.MaxAmount && getSpendAmount > 0 && getReceiveAmount > 0

  protected def longExact(v: BigInt, default: Long): Long              = Try(v.bigInteger.longValueExact()).getOrElse(default)
  protected def minimalAmountOfAmountAssetByPrice(p: Long): Long       = (BigDecimal(Order.PriceConstant) / p).setScale(0, RoundingMode.CEILING).toLong
  protected def correctedAmountOfAmountAsset(min: Long, a: Long): Long = if (min > 0) longExact((BigInt(a) / min) * min, Long.MaxValue) else a

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
    println(s"MatcherModel: LimitOrder.apply(${o.sender}): partialFee: $partialFee")
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
    def counterExecuted: LimitOrder  = counter.partial(amount = executedAmount, fee = counterExecutedFee) // TODO executed?!!!
    def counterRemaining: LimitOrder = counter.partial(amount = counterRemainingAmount, fee = counterRemainingFee)

    def submittedRemainingAmount: Long = math.max(submitted.amount - executedAmount, 0)
    def submittedExecutedFee: Long     = LimitOrder.getPartialFee(submitted.order.matcherFee, submitted.order.amount, executedAmount)
    def submittedRemainingFee: Long    = math.max(submitted.fee - submittedExecutedFee, 0)
    def submittedExecuted: LimitOrder  = submitted.partial(amount = executedAmount, fee = submittedExecutedFee)
    def submittedRemaining: LimitOrder = submitted.partial(amount = submittedRemainingAmount, fee = submittedRemainingFee)
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

  def collectChanges(event: Event): Seq[(Order, OrderInfoDiff)] = {
    event match {
      case OrderAdded(lo) =>
        Seq(
          (lo.order,
           OrderInfoDiff(isNew = true, executedAmount = Some(0L), totalExecutedFee = Some(0L), newMinAmount = Some(lo.minAmountOfAmountAsset))))
      case oe: OrderExecuted =>
        val (o1, o2) = (oe.submittedExecuted, oe.counterExecuted)
        // o1,o2.amount == executed

        println(s"""
                   |collectChanges (from Event):
                   |submitted (id=${oe.submitted.order.id()}) executionAmount(counter): ${oe.submitted.executionAmount(oe.counter)}
                   |submitted.amount: ${o1.amount}/${o1.order.amount}
                   |submitted.amountOfAmountAsset: ${o1.amountOfAmountAsset}
                   |submitted.amountOfPriceAsset: ${o1.amountOfPriceAsset}
                   |submitted fee: ${o1.fee}/${o1.order.matcherFee}
                   |counter (id=${oe.counter.order.id()}) amountOfAmountAsset: ${oe.counter.amountOfAmountAsset}
                   |counter.amount: ${o2.amount}/${o2.order.amount}
                   |counter.amountOfAmountAsset: ${o2.amountOfAmountAsset}
                   |counter.amountOfPriceAsset: ${o2.amountOfPriceAsset}
                   |counter fee: ${o2.fee}/${o2.order.matcherFee}
                   |""".stripMargin)
        Seq(
          // Some(o1.minAmountOfAmountAsset)
          (o1.order,
           OrderInfoDiff(
             isNew = true,
             executedAmount = Some(o1.amountOfAmountAsset),
             totalExecutedFee = Some(o1.fee),
             newMinAmount = Some(o1.minAmountOfAmountAsset)
           )),
          (o2.order,
           OrderInfoDiff(
             executedAmount = Some(o2.amountOfAmountAsset),
             totalExecutedFee = Some(o2.fee),
             newMinAmount = Some(o2.minAmountOfAmountAsset)
           ))
        )
      case OrderCanceled(lo, unmatchable) =>
        Seq((lo.order, OrderInfoDiff(nowCanceled = Some(!unmatchable)))) // !unmatchable?
    }
  }

  def createOpenPortfolio(event: Event): Map[Address, OpenPortfolio] = {
    def overdraftFee(lo: LimitOrder): Long = {
      val feeAmount = LimitOrder.getPartialFee(lo.order.matcherFee, lo.order.amount, lo.amount)
      if (lo.feeAcc == lo.rcvAcc) math.max(feeAmount - lo.getReceiveAmount, 0L) else feeAmount
    }

    event match {
      case OrderAdded(lo) =>
        println(s"""|
              |createOpenPortfolio.OrderAdded:
              |lo.order.id:              ${lo.order.id()}
              |lo.order.senderPublicKey: ${lo.order.senderPublicKey}
              |lo.getSpendAmount: ${lo.getSpendAmount}/${lo.amount}
              |lo.fee:            ${lo.fee}
              |overdraftFee(lo):  ${overdraftFee(lo)}
              |""".stripMargin)
        Map(
          lo.order.senderPublicKey.toAddress -> OpenPortfolio(
            Monoid.combine(
              Map(lo.spentAsset -> lo.getSpendAmount),
              Map(lo.feeAsset   -> overdraftFee(lo))
            )))
      case oe: OrderExecuted =>
        val (o1, o2) = (oe.submittedExecuted, oe.counterExecuted)
        println(s"""|
                    |createOpenPortfolio.OrderExecuted:
                    |o1 (submitted):
                    |o1.order.id:              ${o1.order.id()}
                    |o1.order.senderPublicKey: ${o1.order.senderPublicKey}
                    |o1.getSpendAmount: ${o1.getSpendAmount}/${o1.amount}
                    |o1.fee:            ${o1.fee}
                    |overdraftFee(o1):  ${overdraftFee(o1)}
                    |
                    |o2 (counter):
                    |o2.order.id:              ${o2.order.id()}
                    |o2.order.senderPublicKey: ${o2.order.senderPublicKey}
                    |o2.getSpendAmount: ${o2.getSpendAmount}/${o2.amount}
                    |o2.fee:            ${o2.fee}
                    |overdraftFee(o2):  ${overdraftFee(o2)}
                    |""".stripMargin)

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
        val feeDiff = if (!unmatchable && lo.feeAcc == lo.rcvAcc) math.max(lo.fee - lo.getReceiveAmount, 0L) else lo.fee
        Map(
          lo.order.senderPublicKey.toAddress ->
            OpenPortfolio(
              Monoid.combine(
                Map(lo.spentAsset -> -lo.getSpendAmount),
                Map(lo.feeAsset   -> -feeDiff)
              )))
    }
  }

  def orderInfoDiff(order: Order, prev: OrderInfo, updated: OrderInfo): Map[Address, OpenPortfolio] = Map(
    order.sender.toAddress -> OpenPortfolio(
      Monoid.combine(
        Map(
          order.getSpendAssetId -> (
            LimitOrder.limitOrder(order.price, updated.remaining, updated.remainingFee, order).getSpendAmount -
              LimitOrder.limitOrder(order.price, prev.remaining, prev.remainingFee, order).getSpendAmount
          )
        ), {
          val lo = LimitOrder(order)
          println(s"orderInfoDiff: ${order.sender}: updated.remainingFee=${updated.remainingFee}, prev.remainingFee=${prev.remainingFee}")
          Map(lo.feeAsset -> (if (lo.feeAsset == lo.rcvAsset) 0L else updated.remainingFee - prev.remainingFee))
        }
      )
    )
  )
}

package com.wavesplatform.matcher.model

import cats.Monoid
import cats.implicits._
import com.wavesplatform.database.RW
import com.wavesplatform.matcher.api.DBUtils
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
  def amount: Long // remaining
  def fee: Long    // remaining
  def order: Order
  def partial(amount: Long, fee: Long): LimitOrder

  def getUncorrectedSpendAmount: Long
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
  def getUncorrectedSpendAmount: Long              = amountOfPriceAsset
}

case class SellLimitOrder(price: Price, amount: Long, fee: Long, order: Order) extends LimitOrder {
  def partial(amount: Long, fee: Long): LimitOrder = copy(amount = amount, fee = fee)
  def getReceiveAmount: Long                       = amountOfPriceAsset
  def getSpendAmount: Long                         = amountOfAmountAsset
  def getUncorrectedSpendAmount: Long              = amount
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

    def submittedRemainingAmount: Long = math.max(submitted.amount - executedAmount, 0) // ?
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

  def collectChanges(rw: RW, event: Event): Seq[(Order, OrderInfoDiff)] = {
    event match {
      case OrderAdded(lo) =>
        Seq(
          (lo.order,
           OrderInfoDiff(
             isNew = DBUtils.orderInfo(rw, lo.order.id()) == OrderInfo.empty, // TODO
             addExecutedAmount = Some(0L),
             executedFee = Some(0L),
             newMinAmount = Some(lo.minAmountOfAmountAsset),
             lastSpend = Some(0L)
           )))
      case oe: OrderExecuted =>
        val submitted = oe.submittedExecuted
        val counter   = oe.counterExecuted
        // o1,o2.amount == executed

        println(s"""
                   |collectChanges (from Event):
                   |submitted (id=${oe.submitted.order.id()}) executionAmount(counter): ${oe.submitted.executionAmount(oe.counter)}
                   |submitted.amount: ${submitted.amount}/${submitted.order.amount}
                   |submitted.amountOfAmountAsset: ${submitted.amountOfAmountAsset}
                   |submitted.amountOfPriceAsset: ${submitted.amountOfPriceAsset}
                   |submitted fee: ${submitted.fee}/${submitted.order.matcherFee}
                   |submittedRemaining.getSpendAmount: ${oe.submittedRemaining.getSpendAmount}
                   |
                   |counter (id=${oe.counter.order.id()}) amountOfAmountAsset: ${oe.counter.amountOfAmountAsset}
                   |counter.amount: ${counter.amount}/${counter.order.amount}
                   |counter.amountOfAmountAsset: ${counter.amountOfAmountAsset}
                   |counter.amountOfPriceAsset: ${counter.amountOfPriceAsset}
                   |counterRemaining.getSpendAmount: ${oe.counterRemaining.getSpendAmount}
                   |counter fee: ${counter.fee}/${counter.order.matcherFee}
                   |""".stripMargin)
        Seq(
          // Some(o1.minAmountOfAmountAsset)
          (submitted.order,
           OrderInfoDiff(
             isNew = DBUtils.orderInfo(rw, submitted.order.id()) == OrderInfo.empty, // TODO
             addExecutedAmount = Some(oe.executedAmount),
             executedFee = Some(submitted.fee),
             newMinAmount = Some(submitted.minAmountOfAmountAsset),
             lastSpend = Some(submitted.getSpendAmount)
           )),
          (counter.order,
           OrderInfoDiff(
             isNew = DBUtils.orderInfo(rw, counter.order.id()) == OrderInfo.empty, // TODO
             addExecutedAmount = Some(oe.executedAmount),
             executedFee = Some(counter.fee),
             newMinAmount = Some(counter.minAmountOfAmountAsset),
             lastSpend = Some(counter.getSpendAmount)
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
              |lo.getSpendAmount: ${lo.getSpendAmount}/${lo.getSpendAmount}
              |lo.getReceiveAmount: 0/${lo.getReceiveAmount}
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
                    |o1.isValid:               ${o1.isValid}
                    |o1.order.id:              ${o1.order.id()}
                    |o1.order.senderPublicKey: ${o1.order.senderPublicKey}
                    |o1.getSpendAmount: ${o1.getSpendAmount}/${LimitOrder(oe.submitted.order).getSpendAmount}
                    |o1.fee:            ${o1.fee}
                    |overdraftFee(o1):  ${overdraftFee(o1)}
                    |
                    |o2 (counter):
                    |o2.isValid:               ${o2.isValid}
                    |o2.order.id:              ${o2.order.id()}
                    |o2.order.senderPublicKey: ${o2.order.senderPublicKey}
                    |o2.getSpendAmount: ${o2.getSpendAmount}/${LimitOrder(oe.counter.order).getSpendAmount}
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
        val feeDiff = if (unmatchable) 0L else if (lo.feeAcc == lo.rcvAcc) math.max(lo.fee - lo.getReceiveAmount, 0L) else lo.fee

        Map(
          lo.order.senderPublicKey.toAddress ->
            OpenPortfolio(
              Monoid.combine(
                Map(lo.spentAsset -> -lo.getSpendAmount),
                Map(lo.feeAsset   -> -feeDiff)
              )))
    }
  }

  private def releaseFee(totalReceiveAmount: Long, matcherFee: Long, prevRemaining: Long, updatedRemaining: Long): Long = {
    val executedBefore = matcherFee - prevRemaining
    val restReserved   = math.max(matcherFee - totalReceiveAmount - executedBefore, 0L)

    val executed = prevRemaining - updatedRemaining
    println(s"""|
            |releaseFee:
            |totalReceiveAmount: $totalReceiveAmount
            |matcherFee: $matcherFee
            |prevRemaining: $prevRemaining
            |executedBefore: $executedBefore
            |restReserved: $restReserved
            |executed: $executed
            |""".stripMargin)
    math.min(executed, restReserved)
  }

  private def releaseFee(order: Order, prevRemaining: Long, updatedRemaining: Long): Long = {
    val lo = LimitOrder(order)
    if (lo.rcvAsset == lo.feeAsset) releaseFee(lo.getReceiveAmount, order.matcherFee, prevRemaining, updatedRemaining)
    else prevRemaining - updatedRemaining
  }

  def orderInfoDiffNew(order: Order, oi: OrderInfo): Map[Address, OpenPortfolio] = {
    val lo             = LimitOrder(order)
    val maxSpendAmount = lo.getUncorrectedSpendAmount //lo.getSpendAmount
    val remainingSpend = maxSpendAmount - oi.totalSpend
    val remainingFee   = if (lo.feeAcc == lo.rcvAcc) math.max(oi.remainingFee - lo.getReceiveAmount, 0L) else oi.remainingFee

    println(s"orderInfoDiffNew: remaining spend=$remainingSpend, remaining fee=$remainingFee")
    Map(
      order.sender.toAddress -> OpenPortfolio(
        Monoid.combine(
          Map(order.getSpendAssetId -> remainingSpend),
          Map(lo.feeAsset           -> remainingFee)
        )
      )
    )
  }

  // works only for existed prev
  def orderInfoDiff(order: Order, prev: OrderInfo, updated: OrderInfo): Map[Address, OpenPortfolio] = {
    val lo           = LimitOrder(order)
    val changedSpend = prev.totalSpend - updated.totalSpend
    val changedFee   = -releaseFee(order, prev.remainingFee, updated.remainingFee)
    println(s"orderInfoDiff: changed spend=$changedSpend, fee=$changedFee")
    Map(
      order.sender.toAddress -> OpenPortfolio(
        Monoid.combine(
          Map(order.getSpendAssetId -> changedSpend),
          Map(lo.feeAsset           -> changedFee)
        )
      )
    )
  }

  def orderInfoDiffCancel(order: Order, curr: OrderInfo): Map[Address, OpenPortfolio] = {
    val lo             = LimitOrder(order)
    val maxSpendAmount = lo.getUncorrectedSpendAmount //lo.getSpendAmount
    val remainingSpend = curr.totalSpend - maxSpendAmount
    val remainingFee   = -releaseFee(order, curr.remainingFee, 0)

    println(s"orderInfoDiffCancel: remaining spend=$remainingSpend, remaining fee=$remainingFee")
    Map(
      order.sender.toAddress -> OpenPortfolio(
        Monoid.combine(
          Map(order.getSpendAssetId -> remainingSpend),
          Map(lo.feeAsset           -> remainingFee)
        )
      )
    )
  }
}

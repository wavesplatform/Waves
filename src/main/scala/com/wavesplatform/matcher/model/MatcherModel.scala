package com.wavesplatform.matcher.model

import com.wavesplatform.account.Address
import com.wavesplatform.matcher.model.MatcherModel.Price
import com.wavesplatform.state.{ByteStr, Portfolio}
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.{AssetAcc, AssetId}
import play.api.libs.json.{JsObject, JsValue, Json}

import scala.math.BigDecimal.RoundingMode

object MatcherModel {
  type Price     = Long
  type Level[+A] = Vector[A]
  type OrderId   = String
}

case class LevelAgg(amount: Long, price: Long)

sealed trait LimitOrder extends Product with Serializable {
  def amount: Long // could be remaining or executed, see OrderExecuted
  def price: Price
  def fee: Long // same
  def order: Order
  def partial(amount: Long, fee: Long): LimitOrder

  def getRawSpendAmount: Long // Without correction
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
  def amountOfAmountAsset: Long            = correctedAmountOfAmountAsset(amount, price)
  def executionAmount(o: LimitOrder): Long = correctedAmountOfAmountAsset(amount, o.price)

  def isValid: Boolean =
    amount > 0 && amount >= minAmountOfAmountAsset && amount < Order.MaxAmount && getSpendAmount > 0 && getReceiveAmount > 0

  protected def minimalAmountOfAmountAssetByPrice(p: Long): Long = (BigDecimal(Order.PriceConstant) / p).setScale(0, RoundingMode.CEILING).toLong
  protected def correctedAmountOfAmountAsset(a: Long, p: Long): Long = {
    val settledTotal = (BigDecimal(p) * a / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
    (BigDecimal(settledTotal) / p * Order.PriceConstant).setScale(0, RoundingMode.CEILING).toLong
  }
}

case class BuyLimitOrder(amount: Long, fee: Long, order: Order) extends LimitOrder {
  def price                                           = order.price
  def partial(amount: Long, fee: Long): BuyLimitOrder = copy(amount = amount, fee = fee)
  def getReceiveAmount: Long                          = amountOfAmountAsset
  def getSpendAmount: Long                            = amountOfPriceAsset
  def getRawSpendAmount: Long                         = amountOfPriceAsset
}

case class SellLimitOrder(amount: Long, fee: Long, order: Order) extends LimitOrder {
  def price                                            = order.price
  def partial(amount: Long, fee: Long): SellLimitOrder = copy(amount = amount, fee = fee)
  def getReceiveAmount: Long                           = amountOfPriceAsset
  def getSpendAmount: Long                             = amountOfAmountAsset
  def getRawSpendAmount: Long                          = amount
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
    def json: JsObject   = Json.obj("status" -> name, "message" -> "The limit order is not found")
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
      case OrderType.BUY  => BuyLimitOrder(o.amount, partialFee, o)
      case OrderType.SELL => SellLimitOrder(o.amount, partialFee, o)
    }
  }

  def limitOrder(remainingAmount: Long, remainingFee: Long, o: Order): LimitOrder = {
    o.orderType match {
      case OrderType.BUY  => BuyLimitOrder(remainingAmount, remainingFee, o)
      case OrderType.SELL => SellLimitOrder(remainingAmount, remainingFee, o)
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

    def counterRemainingAmount: Long            = math.max(counter.amount - executedAmount, 0)
    def counterExecutedFee: Long                = LimitOrder.getPartialFee(counter.order.matcherFee, counter.order.amount, executedAmount)
    def counterRemainingFee: Long               = math.max(counter.fee - counterExecutedFee, 0)
    def counterExecuted: LimitOrder             = counter.partial(amount = executedAmount, fee = counterExecutedFee)
    def counterRemaining: LimitOrder            = counter.partial(amount = counterRemainingAmount, fee = counterRemainingFee)
    def counterRemainingOpt: Option[LimitOrder] = if (counterRemainingAmount <= 0) None else Some(counterRemaining)

    def submittedRemainingAmount: Long            = math.max(submitted.amount - executedAmount, 0)
    def submittedExecutedFee: Long                = LimitOrder.getPartialFee(submitted.order.matcherFee, submitted.order.amount, executedAmount)
    def submittedRemainingFee: Long               = math.max(submitted.fee - submittedExecutedFee, 0)
    def submittedExecuted: LimitOrder             = submitted.partial(amount = executedAmount, fee = submittedExecutedFee)
    def submittedRemaining: LimitOrder            = submitted.partial(amount = submittedRemainingAmount, fee = submittedRemainingFee)
    def submittedRemainingOpt: Option[LimitOrder] = if (submittedRemainingAmount <= 0) None else Some(submittedRemaining)
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
}

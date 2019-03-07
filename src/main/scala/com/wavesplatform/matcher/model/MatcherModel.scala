package com.wavesplatform.matcher.model

import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.account.Address
import com.wavesplatform.matcher.model.MatcherModel.Price
import com.wavesplatform.state.Portfolio
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange._
import play.api.libs.json.{JsObject, JsValue, Json}

import scala.math.BigDecimal.RoundingMode

object MatcherModel {
  type Price = Long
}

case class LevelAgg(amount: Long, price: Long)

sealed trait LimitOrder {
  def amount: Long // could be remaining or executed, see OrderExecuted
  def fee: Long    // same
  def order: Order

  def price: Price = order.price
  def partial(amount: Long, fee: Long): LimitOrder

  protected def rawSpendAmount: Long // Without correction
  protected def spendAmount: Long
  protected def receiveAmount: Long

  def spentAsset: Asset = order.getSpendAssetId
  def rcvAsset: Asset   = order.getReceiveAssetId
  val feeAsset: Asset   = Waves

  def requiredBalance: Map[Asset, Long] = Monoid.combine(
    Map(spentAsset -> rawSpendAmount),
    Map(feeAsset   -> (if (feeAsset != rcvAsset) fee else (fee - receiveAmount).max(0L)))
  )

  def amountOfPriceAsset: Long                           = (BigDecimal(amount) * price / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
  def amountOfAmountAsset: Long                          = correctedAmountOfAmountAsset(amount, price)
  private def executionAmount(counterPrice: Price): Long = correctedAmountOfAmountAsset(amount, counterPrice)

  def isValid: Boolean = isValid(price)
  def isValid(counterPrice: Price): Boolean =
    amount > 0 && amount >= minimalAmountOfAmountAssetByPrice(counterPrice) && amount < Order.MaxAmount && spendAmount > 0 && receiveAmount > 0

  private def minimalAmountOfAmountAssetByPrice(p: Long): Long = (BigDecimal(Order.PriceConstant) / p).setScale(0, RoundingMode.CEILING).toLong
  private def correctedAmountOfAmountAsset(a: Long, p: Long): Long = {
    val settledTotal = (BigDecimal(p) * a / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
    (BigDecimal(settledTotal) / p * Order.PriceConstant).setScale(0, RoundingMode.CEILING).toLong
  }
}

case class BuyLimitOrder(amount: Long, fee: Long, order: Order) extends LimitOrder {
  def partial(amount: Long, fee: Long): BuyLimitOrder = copy(amount = amount, fee = fee)
  def receiveAmount: Long                             = amountOfAmountAsset
  def spendAmount: Long                               = amountOfPriceAsset
  def rawSpendAmount: Long                            = amountOfPriceAsset
  override def toString                               = s"BuyLimitOrder($amount,$fee,${order.id()})"
}

case class SellLimitOrder(amount: Long, fee: Long, order: Order) extends LimitOrder {
  def partial(amount: Long, fee: Long): SellLimitOrder = copy(amount = amount, fee = fee)
  def receiveAmount: Long                              = amountOfPriceAsset
  def spendAmount: Long                                = amountOfAmountAsset
  def rawSpendAmount: Long                             = amount
  override def toString                                = s"SellLimitOrder($amount,$fee,${order.id()})"
}

object LimitOrder {
  def apply(o: Order): LimitOrder = {
    val pf = partialFee(o.matcherFee, o.amount, o.amount)
    o.orderType match {
      case OrderType.BUY  => BuyLimitOrder(o.amount, pf, o)
      case OrderType.SELL => SellLimitOrder(o.amount, pf, o)
    }
  }

  def partialFee(matcherFee: Long, totalAmount: Long, partialAmount: Long): Long = {
    // Should not round! It could lead to forks. See ExchangeTransactionDiff
    (BigInt(matcherFee) * partialAmount / totalAmount).toLong
  }

  def executedAmount(submitted: LimitOrder, counter: LimitOrder): Long =
    math.min(submitted.executionAmount(counter.price), counter.amountOfAmountAsset)
}

sealed trait OrderStatus {
  def name: String
  def json: JsValue
}

object OrderStatus {
  sealed trait Final extends OrderStatus

  case object Accepted extends OrderStatus {
    val name           = "Accepted"
    def json: JsObject = Json.obj("status" -> name)
  }
  case object NotFound extends Final {
    val name           = "NotFound"
    def json: JsObject = Json.obj("status" -> name, "message" -> "The limit order is not found")
  }
  case class PartiallyFilled(filled: Long) extends OrderStatus {
    val name           = "PartiallyFilled"
    def json: JsObject = Json.obj("status" -> name, "filledAmount" -> filled)
  }
  case class Filled(filled: Long) extends Final {
    val name           = "Filled"
    def json: JsObject = Json.obj("status" -> name, "filledAmount" -> filled)
  }
  case class Cancelled(filled: Long) extends Final {
    val name           = "Cancelled"
    def json: JsObject = Json.obj("status" -> name, "filledAmount" -> filled)
  }

  def finalStatus(lo: LimitOrder, unmatchable: Boolean): Final = {
    val filledAmount = lo.order.amount - lo.amount
    if (unmatchable && filledAmount > 0) Filled(filledAmount) else Cancelled(filledAmount)
  }
}

object Events {

  sealed trait Event

  case class OrderExecuted(submitted: LimitOrder, counter: LimitOrder, timestamp: Long) extends Event {
    lazy val executedAmount: Long = LimitOrder.executedAmount(submitted, counter)

    def counterRemainingAmount: Long = math.max(counter.amount - executedAmount, 0)
    def counterExecutedFee: Long     = LimitOrder.partialFee(counter.order.matcherFee, counter.order.amount, executedAmount)
    def counterRemainingFee: Long    = math.max(counter.fee - counterExecutedFee, 0)
    def counterRemaining: LimitOrder = counter.partial(amount = counterRemainingAmount, fee = counterRemainingFee)

    def submittedRemainingAmount: Long = math.max(submitted.amount - executedAmount, 0)
    def submittedExecutedFee: Long     = LimitOrder.partialFee(submitted.order.matcherFee, submitted.order.amount, executedAmount)
    def submittedRemainingFee: Long    = math.max(submitted.fee - submittedExecutedFee, 0)
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
    case class Changes(updatedPortfolio: Portfolio, changedAssets: Set[Option[Asset]])
  }
}

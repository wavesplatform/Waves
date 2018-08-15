package com.wavesplatform.matcher.model

import cats.instances.map._
import cats.syntax.semigroup._
import cats.{Monoid, Semigroup}
import com.wavesplatform.transaction.AssetId
import play.api.libs.json.{Format, Json}

import scala.util.Try

case class OrderInfo(amount: Long, filled: Long, canceled: Boolean, minAmount: Option[Long], remainingFee: Long, unsafeTotalSpend: Option[Long]) {
  def remaining: Long = if (canceled) 0L else amount - filled

  /**
    * TODO: Remove in future
    * @param orig Original means LimitOrder(order) without any partial fills
    * @return
    */
  def totalSpend(orig: LimitOrder): Long =
    unsafeTotalSpend.getOrElse(orig.getRawSpendAmount - orig.partial(filled, orig.order.matcherFee - remainingFee).getSpendAmount)

  def status: LimitOrder.OrderStatus = {
    if (amount == 0) LimitOrder.NotFound
    else if (canceled) LimitOrder.Cancelled(filled)
    else if (filled == 0) LimitOrder.Accepted
    else if (filled < amount - minAmount.getOrElse(0L)) LimitOrder.PartiallyFilled(filled)
    else LimitOrder.Filled(filled)
  }

  def jsonStr: String = {
    Json.stringify(Json.toJson(this))
  }
}

object OrderInfo {
  def safeSum(x: Long, y: Long): Long         = Try(Math.addExact(x, y)).getOrElse(Long.MaxValue)
  implicit val longSemigroup: Semigroup[Long] = (x: Long, y: Long) => safeSum(x, y)

  val empty = OrderInfo(0L, 0L, false, None, 0L, Some(0L))

  implicit val orderInfoFormat: Format[OrderInfo] = Json.format[OrderInfo]

  implicit val orderStatusOrdering: Ordering[LimitOrder.OrderStatus] = (first: LimitOrder.OrderStatus, second: LimitOrder.OrderStatus) => {
    implicitly[Ordering[Int]].compare(first.ordering, second.ordering)
  }
}

case class OpenPortfolio(orders: Map[Option[AssetId], Long])

object OpenPortfolio {
  import OrderInfo.longSemigroup
  val empty = OpenPortfolio(Map())

  implicit val orderPortfolioMonoid = new Monoid[OpenPortfolio] {
    override def empty: OpenPortfolio = OpenPortfolio.empty

    override def combine(lhs: OpenPortfolio, rhs: OpenPortfolio): OpenPortfolio = {
      OpenPortfolio(lhs.orders.combine(rhs.orders))
    }
  }

}

package com.wavesplatform.matcher.model

import cats.instances.map._
import cats.syntax.semigroup._
import cats.{Monoid, Semigroup}
import play.api.libs.json.{Format, Json}
import scorex.transaction.AssetId

import scala.util.Try

case class OrderInfo(amount: Long, filled: Long, canceled: Boolean) {
  def remaining: Long = if (canceled) 0L else amount - filled

  def status: LimitOrder.OrderStatus = {
    if (amount == 0) LimitOrder.NotFound
    else if (canceled) LimitOrder.Cancelled(filled)
    else if (filled == 0) LimitOrder.Accepted
    else if (filled < amount) LimitOrder.PartiallyFilled(filled)
    else LimitOrder.Filled
  }

  def jsonStr: String = {
    Json.stringify(Json.toJson(this))
  }
}

object OrderInfo {
  def safeSum(x: Long, y: Long): Long         = Try(Math.addExact(x, y)).getOrElse(Long.MaxValue)
  implicit val longSemigroup: Semigroup[Long] = (x: Long, y: Long) => safeSum(x, y)

  val empty = OrderInfo(0L, 0L, false)
  implicit val orderInfoMonoid = new Monoid[OrderInfo] {
    override def empty: OrderInfo = OrderInfo.empty

    override def combine(older: OrderInfo, newer: OrderInfo): OrderInfo =
      OrderInfo(
        math.max(older.amount, newer.amount),
        older.filled.combine(newer.filled),
        newer.canceled
      )
  }

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

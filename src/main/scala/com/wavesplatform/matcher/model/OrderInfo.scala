package com.wavesplatform.matcher.model

import cats.instances.map._
import cats.syntax.semigroup._
import cats.{Monoid, Semigroup}
import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.{Format, Json, OFormat}
import scorex.utils.Booleans

import scala.util.Try


case class OrderInfo(amount: Long, filled: Long, canceled: Boolean) {
  def remaining: Long = if (!canceled) amount - filled else 0L

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
  def limitSum(x: Long, y: Long): Long = Try(math.max(0L, Math.addExact(x, y))).getOrElse(Long.MaxValue)
  implicit val limitSemigroup: Semigroup[Long] = (x: Long, y: Long) => limitSum(x, y)

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
}

case class OpenPortfolio(orders: Map[String, Long])

object OpenPortfolio {
  import OrderInfo.limitSemigroup
  val empty = OpenPortfolio(Map())
  def limitSubstract(x: Long, y: Long): Long = math.max(0L, x - y)

  implicit val orderPortfolioMonoid = new Monoid[OpenPortfolio] {
    override def empty: OpenPortfolio = OpenPortfolio.empty

    override def combine(lhs: OpenPortfolio, rhs: OpenPortfolio): OpenPortfolio = {
      OpenPortfolio(lhs.orders.combine(rhs.orders))
    }
  }

}

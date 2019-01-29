package com.wavesplatform.state

import cats._
import cats.kernel.instances.map._
import com.wavesplatform.block.Block.Fraction
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.AssetId

case class Portfolio(balance: Long, lease: LeaseBalance, assets: Map[ByteStr, Long]) {
  lazy val effectiveBalance: Long = safeSum(balance, lease.in) - lease.out
  lazy val spendableBalance: Long = balance - lease.out

  lazy val isEmpty: Boolean = this == Portfolio.empty

  def balanceOf(assetId: Option[AssetId]): Long = assetId.fold(balance)(assets.getOrElse(_, 0))
  def remove(assetId: Option[AssetId], amount: Long): Option[Portfolio] = {
    val origAmount = assetId match {
      case None    => balance
      case Some(x) => assets.getOrElse(x, 0L)
    }

    val updatedAmount = origAmount - amount
    if (updatedAmount < 0) None
    else
      Some(assetId match {
        case None    => copy(balance = updatedAmount)
        case Some(x) => copy(assets = this.assets.updated(x, updatedAmount))
      })
  }
}

object Portfolio {

  def build(a: Option[AssetId], amount: Long): Portfolio = a match {
    case None    => Portfolio(amount, LeaseBalance.empty, Map.empty)
    case Some(t) => Portfolio(0L, LeaseBalance.empty, Map(t -> amount))
  }

  val empty = Portfolio(0L, Monoid[LeaseBalance].empty, Map.empty)

  implicit val longSemigroup: Semigroup[Long] = (x: Long, y: Long) => safeSum(x, y)

  implicit val monoid: Monoid[Portfolio] = new Monoid[Portfolio] {
    override val empty: Portfolio = Portfolio.empty

    override def combine(older: Portfolio, newer: Portfolio): Portfolio =
      Portfolio(balance = safeSum(older.balance, newer.balance),
                lease = Monoid.combine(older.lease, newer.lease),
                assets = Monoid.combine(older.assets, newer.assets))
  }

  implicit class PortfolioExt(self: Portfolio) {

    def pessimistic: Portfolio = Portfolio(
      balance = Math.min(self.balance, 0),
      lease = LeaseBalance(
        in = 0,
        out = Math.max(self.lease.out, 0)
      ),
      assets = self.assets.filter { case (_, v) => v < 0 }
    )

    def multiply(f: Fraction): Portfolio =
      Portfolio(f(self.balance), LeaseBalance.empty, self.assets.mapValues(f.apply))

    def minus(other: Portfolio): Portfolio =
      Portfolio(self.balance - other.balance, LeaseBalance.empty, Monoid.combine(self.assets, other.assets.mapValues(-_)))

    def negate = Portfolio.empty minus self
  }

}

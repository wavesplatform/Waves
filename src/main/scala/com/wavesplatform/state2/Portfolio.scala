package com.wavesplatform.state2

import cats._
import cats.kernel.instances.map._
import cats.Monoid
import scorex.block.Block

case class Portfolio(balance: Long, leaseInfo: LeaseInfo, assets: Map[ByteStr, Long]) {
  lazy val effectiveBalance: Long = safeSum(balance, leaseInfo.leaseIn) - leaseInfo.leaseOut
  lazy val spendableBalance: Long = balance - leaseInfo.leaseOut

  lazy val prevBlockFeePart: Portfolio = this.minus(this.multiply(Block.CurrentBlockFee))
  lazy val isEmpty: Boolean = this == Portfolio.portfolioMonoid.empty
}

object Portfolio {

  implicit val longSemigroup: Semigroup[Long] = (x: Long, y: Long) => safeSum(x, y)

  implicit val portfolioMonoid = new Monoid[Portfolio] {
    override val empty: Portfolio = Portfolio(0L, Monoid[LeaseInfo].empty, Map.empty)

    override def combine(older: Portfolio, newer: Portfolio): Portfolio
    = Portfolio(
      balance = safeSum(older.balance, newer.balance),
      leaseInfo = Monoid.combine(older.leaseInfo, newer.leaseInfo),
      assets = Monoid.combine(older.assets, newer.assets))
  }

  implicit class PortfolioExt(self: Portfolio) {

    def pessimistic: Portfolio = Portfolio(
      balance = Math.min(self.balance, 0),
      leaseInfo = LeaseInfo(
        leaseIn = 0,
        leaseOut = Math.max(self.leaseInfo.leaseOut, 0)
      ),
      assets = self.assets.filter { case (_, v) => v < 0 }
    )

    def multiply(m: Float): Portfolio =
      Portfolio((self.balance * m).toLong, LeaseInfo((self.leaseInfo.leaseIn * m).toLong, (self.leaseInfo.leaseOut * m).toLong), self.assets.mapValues(v => (v * m).toLong))

    def minus(other: Portfolio): Portfolio =
      Portfolio(self.balance - other.balance, LeaseInfo(self.leaseInfo.leaseIn - other.leaseInfo.leaseIn, self.leaseInfo.leaseOut - other.leaseInfo.leaseOut),
        Monoid.combine(self.assets, other.assets.mapValues(-_)))
  }

}

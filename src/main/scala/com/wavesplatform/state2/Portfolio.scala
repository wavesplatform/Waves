package com.wavesplatform.state2

import cats._
import cats.kernel.instances.map._
import cats.Monoid

case class Portfolio(balance: Long, leaseInfo: LeaseInfo, assets: Map[ByteStr, Long]) {
  lazy val effectiveBalance: Long = safeSum(balance, leaseInfo.leaseIn) - leaseInfo.leaseOut
}

object Portfolio {
  implicit val longSemigroup: Semigroup[Long] = (x: Long, y: Long) => safeSum(x, y)

  implicit val portfolioMonoid = new Monoid[Portfolio] {
    override def empty: Portfolio = Portfolio(0L, Monoid[LeaseInfo].empty, Map.empty)

    override def combine(older: Portfolio, newer: Portfolio): Portfolio
    = Portfolio(
      balance = safeSum(older.balance, newer.balance),
      leaseInfo = Monoid.combine(older.leaseInfo, newer.leaseInfo),
      assets = Monoid.combine(older.assets, newer.assets))
  }
}

package com.wavesplatform.state2

import cats._
import cats.kernel.instances.map._
import cats.Monoid

case class Portfolio(balance: Long, leaseInfo: LeaseInfo, assets: Map[ByteStr, Long]) {
  lazy val effectiveBalance: Long = safeSum(balance, leaseInfo.leaseIn) - leaseInfo.leaseOut
  lazy val spendableBalance: Long = balance - leaseInfo.leaseOut

  /**
    * The Portfolio with only the withdraw part
    */
  def pessimistic: Portfolio = Portfolio(
    balance = Math.min(balance, 0),
    leaseInfo = LeaseInfo(
      leaseIn = 0,
      leaseOut = Math.max(leaseInfo.leaseOut, 0)
    ),
    assets = assets.filter { case (_, v) => v < 0 }
  )

  def multiply(m: Float): Portfolio =
    Portfolio((balance * m).toLong, LeaseInfo((leaseInfo.leaseIn * m).toLong, (leaseInfo.leaseOut * m).toLong), assets.mapValues(v => (v * m).toLong))

  def isEmpty: Boolean = this == Monoid.empty[Portfolio]
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

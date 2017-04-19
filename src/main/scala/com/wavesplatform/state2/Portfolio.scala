package com.wavesplatform.state2

import cats._
import cats.implicits._
import cats.Monoid

case class Portfolio(balance: Long, leaseInfo: LeaseInfo, assets: Map[ByteArray, Long]) {
  lazy val effectiveBalance: Long = safeSum(balance, leaseInfo.leaseIn) - leaseInfo.leaseOut
}

object Portfolio {
  implicit val portfolioMonoid = new Monoid[Portfolio] {
    override def empty: Portfolio = Portfolio(0L, Monoid[LeaseInfo].empty, Map.empty)

    override def combine(older: Portfolio, newer: Portfolio): Portfolio
    = Portfolio(
      balance = safeSum(older.balance, newer.balance),
      leaseInfo = Monoid.combine(older.leaseInfo, newer.leaseInfo),
      assets = (older.assets.keys ++ newer.assets.keys)
        .map(ba => ba -> safeSum(older.assets.getOrElse(ba, 0), newer.assets.getOrElse(ba, 0)))
        .toMap)
  }
}
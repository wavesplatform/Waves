package com.wavesplatform.state

import cats._
import cats.kernel.instances.map._
import com.wavesplatform.state.diffs.BlockDiffer.Fraction
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset._

import scala.collection.immutable.Map

case class Portfolio(balance: Long = 0L, lease: LeaseBalance = LeaseBalance.empty, assets: Map[IssuedAsset, Long] = Map.empty) {
  lazy val effectiveBalance: Long = safeSum(balance, lease.in) - lease.out
  lazy val spendableBalance: Long = balance - lease.out

  lazy val isEmpty: Boolean = this == Portfolio.empty

  def balanceOf(assetId: Asset): Long = assetId match {
    case Waves                  => balance
    case asset @ IssuedAsset(_) => assets.getOrElse(asset, 0L)
  }
}

object Portfolio {
  def waves(amount: Long): Portfolio = build(Waves, amount)

  def build(af: (Asset, Long)): Portfolio = build(af._1, af._2)

  def build(a: Asset, amount: Long): Portfolio = a match {
    case Waves              => Portfolio(amount)
    case t @ IssuedAsset(_) => Portfolio(assets = Map(t -> amount))
  }

  val empty: Portfolio = Portfolio()

  implicit val longSemigroup: Semigroup[Long] = (x: Long, y: Long) => safeSum(x, y)

  implicit val monoid: Monoid[Portfolio] = new Monoid[Portfolio] {
    override val empty: Portfolio = Portfolio.empty

    override def combine(older: Portfolio, newer: Portfolio): Portfolio =
      Portfolio(
        balance = safeSum(older.balance, newer.balance),
        lease = Monoid.combine(older.lease, newer.lease),
        assets = Monoid.combine(older.assets, newer.assets)
      )
  }

  implicit class PortfolioExt(val self: Portfolio) extends AnyVal {
    def spendableBalanceOf(assetId: Asset): Long = assetId.fold(self.spendableBalance)(self.assets.getOrElse(_, 0L))

    def pessimistic: Portfolio = Portfolio(
      balance = Math.min(self.balance, 0),
      lease = LeaseBalance(
        in = 0,
        out = Math.max(self.lease.out, 0)
      ),
      assets = self.assets.filter { case (_, v) => v < 0 }
    )

    def multiply(f: Fraction): Portfolio =
      Portfolio(f(self.balance), LeaseBalance.empty, self.assets.view.mapValues(f.apply).toMap)

    def minus(other: Portfolio): Portfolio =
      Portfolio(self.balance - other.balance, LeaseBalance.empty, Monoid.combine(self.assets, other.assets.view.mapValues(-_).toMap))

    def negate: Portfolio = Portfolio.empty minus self

    def assetIds: Set[Asset] = self.assets.keySet ++ Set(Waves)

    def changedAssetIds(that: Portfolio): Set[Asset] = {
      val a1 = assetIds
      val a2 = that.assetIds

      val intersection = a1 & a2
      val sureChanged  = (a1 | a2) -- intersection
      intersection.filter(x => spendableBalanceOf(x) != that.spendableBalanceOf(x)) ++ sureChanged
    }
  }
}

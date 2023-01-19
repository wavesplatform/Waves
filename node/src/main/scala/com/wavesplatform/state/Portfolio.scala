package com.wavesplatform.state

import com.wavesplatform.state.diffs.BlockDiffer.Fraction
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.*

import scala.collection.immutable.VectorMap

case class Portfolio(balance: Long = 0L, lease: LeaseBalance = LeaseBalance.empty, assets: VectorMap[IssuedAsset, Long] = VectorMap.empty) {
  import Portfolio.*
  lazy val effectiveBalance: Either[String, Long] = safeSum(balance, lease.in, "Effective balance").map(_ - lease.out)
  lazy val spendableBalance: Long                 = balance - lease.out

  lazy val isEmpty: Boolean = this == Portfolio.empty

  @inline
  final def balanceOf(assetId: Asset): Long = if (assetId eq Waves) balance else assets.getOrElse(assetId.asInstanceOf[IssuedAsset], 0L)

  def combine(that: Portfolio): Either[String, Portfolio] =
    for {
      balance  <- sum(this.balance, that.balance, "Waves balance sum overflow")
      assets   <- combineAssets(this.assets, that.assets)
      leaseIn  <- sum(this.lease.in, that.lease.in, "Lease in sum overflow")
      leaseOut <- sum(this.lease.out, that.lease.out, "Lease out sum overflow")
    } yield Portfolio(balance, LeaseBalance(leaseIn, leaseOut), assets)

  override def toString: String = s"PF($balance,${assets.mkString("[", ",", "]")})"
}

object Portfolio {
  @inline
  final def sum(a: Long, b: Long, error: => String): Either[String, Long] =
    try Right(Math.addExact(a, b))
    catch { case _: ArithmeticException => Left(error) }

  def combineAssets(a: VectorMap[IssuedAsset, Long], b: VectorMap[IssuedAsset, Long]): Either[String, VectorMap[IssuedAsset, Long]] = {
    if (a.isEmpty) Right(b)
    else if (b.isEmpty) Right(a)
    else
      b.foldLeft[Either[String, VectorMap[IssuedAsset, Long]]](Right(a)) {
        case (Right(seed), (asset, balance)) =>
          seed.get(asset) match {
            case None =>
              Right(seed.updated(asset, balance))
            case Some(oldBalance) =>
              sum(oldBalance, balance, s"asset $asset overflow").map { newBalance =>
                seed.updated(asset, newBalance)
              }
          }
        case (left, _) => left
      }
  }

  private def unsafeCombineAssets(a: VectorMap[IssuedAsset, Long], b: VectorMap[IssuedAsset, Long]): VectorMap[IssuedAsset, Long] =
    if (a.isEmpty) b
    else if (b.isEmpty) a
    else
      b.foldLeft(a) { case (seed, (asset, balance)) =>
        val newBalance = seed.get(asset).fold(balance)(_ + balance)
        seed.updated(asset, newBalance)
      }

  def waves(amount: Long): Portfolio = build(Waves, amount)

  def build(af: (Asset, Long)): Portfolio = build(af._1, af._2)

  def build(a: Asset, amount: Long): Portfolio = a match {
    case Waves              => Portfolio(amount)
    case t @ IssuedAsset(_) => Portfolio(assets = VectorMap(t -> amount))
  }

  def build(wavesAmount: Long, a: IssuedAsset, amount: Long): Portfolio = Portfolio(wavesAmount, assets = VectorMap(a -> amount))

  val empty: Portfolio = Portfolio()

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
      Portfolio(f(self.balance), LeaseBalance.empty, self.assets.view.mapValues(f.apply).to(VectorMap))

    def minus(other: Portfolio): Portfolio =
      Portfolio(self.balance - other.balance, LeaseBalance.empty, unsafeCombineAssets(self.assets, other.assets.view.mapValues(- _).to(VectorMap)))

    def negate: Portfolio = Portfolio.empty minus self

    def assetIds: Set[Asset] = self.assets.keySet ++ Set[Asset](Waves)
  }
}

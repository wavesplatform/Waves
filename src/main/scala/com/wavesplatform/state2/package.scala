package com.wavesplatform

import cats._
import cats.implicits._
import cats.Monoid
import scorex.crypto.encode.Base58

import scala.util.Try

package object state2 {

  case class EqByteArray(arr: Array[Byte]) {
    override def equals(a: Any): Boolean = a match {
      case eba: EqByteArray => arr.sameElements(eba.arr)
      case _ => false
    }

    override def hashCode(): Int = java.util.Arrays.hashCode(arr)

    override lazy val toString: String = "ByteArray:" + Base58.encode(arr)

  }

  type ByteArray = EqByteArray

  private def safeSum(x: Long, y: Long): Long = Try(Math.addExact(x, y)).getOrElse(Long.MinValue)

  implicit val portfolioMonoid = new Monoid[Portfolio] {
    override def empty: Portfolio = Portfolio(0L, 0L, Map.empty)

    override def combine(older: Portfolio, newer: Portfolio): Portfolio
    = Portfolio(
      balance = safeSum(older.balance, newer.balance),
      effectiveBalance = safeSum(older.effectiveBalance, newer.effectiveBalance),
      assets = (older.assets.keys ++ newer.assets.keys)
        .map(ba => ba -> safeSum(older.assets.getOrElse(ba, 0), newer.assets.getOrElse(ba, 0)))
        .toMap)
  }

  implicit val assetInfoMonoid = new Monoid[AssetInfo] {
    override def empty: AssetInfo = AssetInfo(isReissuable = true, 0)

    override def combine(x: AssetInfo, y: AssetInfo): AssetInfo
    = AssetInfo(x.isReissuable && y.isReissuable, x.volume + y.volume)
  }

  implicit val diffMonoid = new Monoid[Diff] {
    override def empty: Diff = Diff(Map.empty, Map.empty, Map.empty)

    override def combine(older: Diff, newer: Diff): Diff = Diff(
      transactions = newer.transactions ++ older.transactions,
      portfolios = older.portfolios.combine(newer.portfolios),
      issuedAssets = newer.issuedAssets.combine(older.issuedAssets))
  }

  implicit val blockDiffMonoid = new Monoid[BlockDiff] {
    override def empty: BlockDiff = BlockDiff(diffMonoid.empty, 0, Seq.empty)

    override def combine(older: BlockDiff, newer: BlockDiff): BlockDiff = BlockDiff(
      txsDiff = older.txsDiff.combine(newer.txsDiff),
      heightDiff = older.heightDiff + newer.heightDiff,
      effectiveBalanceSnapshots = older.effectiveBalanceSnapshots ++ newer.effectiveBalanceSnapshots)
  }
}

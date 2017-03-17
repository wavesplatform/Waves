package com.wavesplatform

import cats._
import cats.implicits._
import cats.Monoid

package object state2 {

  case class EqByteArray(arr: Array[Byte]) {
    override def equals(a: Any): Boolean = a match {
      case eba: EqByteArray => arr.sameElements(eba.arr)
      case _ => false
    }

    override def hashCode(): Int = java.util.Arrays.hashCode(arr)
  }

  type ByteArray = EqByteArray

  implicit val portfolioMonoid = new Monoid[Portfolio] {
    override def empty: Portfolio = Portfolio(0L, 0L, Map.empty)

    override def combine(older: Portfolio, newer: Portfolio): Portfolio
    = Portfolio(older.balance + newer.balance, older.effectiveBalance + newer.effectiveBalance, older.assets.combine(newer.assets))
  }

  implicit val diffMonoid = new Monoid[Diff] {
    override def empty: Diff = Diff(Map.empty, Map.empty, Map.empty)

    override def combine(older: Diff, newer: Diff): Diff = Diff(
      transactions = older.transactions ++ newer.transactions,
      portfolios = older.portfolios.combine(newer.portfolios),
      issuedAssets = newer.issuedAssets ++ older.issuedAssets)
  }

  implicit val blockDiffMonoid = new Monoid[BlockDiff] {
    override def empty: BlockDiff = BlockDiff(diffMonoid.empty, 0, Seq.empty)

    override def combine(older: BlockDiff, newer: BlockDiff): BlockDiff = BlockDiff(
      txsDiff = older.txsDiff.combine(newer.txsDiff),
      heightDiff = older.heightDiff + newer.heightDiff,
      effectiveBalanceSnapshots = newer.effectiveBalanceSnapshots ++ older.effectiveBalanceSnapshots)
  }
}

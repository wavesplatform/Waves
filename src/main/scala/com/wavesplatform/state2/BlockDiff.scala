package com.wavesplatform.state2

import cats._
import cats.implicits._
import cats.Monoid

case class BlockDiff(txsDiff: Diff,
                     heightDiff: Int,
                     effectiveBalanceSnapshots: Seq[EffectiveBalanceSnapshot])

object BlockDiff {
  implicit val blockDiffMonoid = new Monoid[BlockDiff] {
    override def empty: BlockDiff = BlockDiff(Monoid[Diff].empty, 0, Seq.empty)

    override def combine(older: BlockDiff, newer: BlockDiff): BlockDiff = BlockDiff(
      txsDiff = older.txsDiff.combine(newer.txsDiff),
      heightDiff = older.heightDiff + newer.heightDiff,
      effectiveBalanceSnapshots = newer.effectiveBalanceSnapshots ++ older.effectiveBalanceSnapshots)
  }
}
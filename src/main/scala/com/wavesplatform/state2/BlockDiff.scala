package com.wavesplatform.state2

import cats._
import cats.implicits._
import cats.Monoid
import scorex.account.Account

case class BlockDiff(txsDiff: Diff,
                     heightDiff: Int,
                     updates: Map[Account, Map[Int, Snapshot]])

object BlockDiff {
  implicit val blockDiffMonoid = new Monoid[BlockDiff] {
    override def empty: BlockDiff = BlockDiff(Monoid[Diff].empty, 0, Map.empty)

    override def combine(older: BlockDiff, newer: BlockDiff): BlockDiff = BlockDiff(
      txsDiff = older.txsDiff.combine(newer.txsDiff),
      heightDiff = older.heightDiff + newer.heightDiff,
      updates = newer.updates.combine(older.updates))
  }
}
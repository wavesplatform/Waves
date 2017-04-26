package com.wavesplatform.state2

import cats._
import cats.implicits._
import cats.Monoid
import scorex.account.Account

import scala.collection.SortedMap

case class BlockDiff(txsDiff: Diff,
                     heightDiff: Int,
                     snapshots: Map[Account, SortedMap[Int, Snapshot]])

object BlockDiff {

  implicit def sortedMapForSnapshotsMonoid[Int, Snapshot]: Monoid[SortedMap[Int, Snapshot]] = new Monoid[SortedMap[Int, Snapshot]] {
    def empty: SortedMap[Int, Snapshot] = SortedMap.empty[Int, Snapshot]
    def combine(f1: SortedMap[Int, Snapshot], f2: SortedMap[Int, Snapshot]): SortedMap[Int, Snapshot] = f1 ++ f2
  }

  implicit val blockDiffMonoid = new Monoid[BlockDiff] {
    override def empty: BlockDiff = BlockDiff(Monoid[Diff].empty, 0, Map.empty)

    override def combine(older: BlockDiff, newer: BlockDiff): BlockDiff = BlockDiff(
      txsDiff = older.txsDiff.combine(newer.txsDiff),
      heightDiff = older.heightDiff + newer.heightDiff,
      snapshots = older.snapshots.combine(newer.snapshots))
  }
}
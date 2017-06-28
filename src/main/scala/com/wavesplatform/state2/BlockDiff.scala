package com.wavesplatform.state2

import cats.Monoid
import cats.implicits._
import scorex.account.Account

import scala.collection.SortedMap

case class BlockDiff(txsDiff: Diff,
                     heightDiff: Int,
                     snapshots: Map[Account, SortedMap[Int, Snapshot]])

object BlockDiff {

  val empty: BlockDiff = BlockDiff(Monoid[Diff].empty, 0, Map.empty)

  implicit def sortedMapForSnapshotsMonoid[A: Ordering, Snapshot]: Monoid[SortedMap[A, Snapshot]] = new Monoid[SortedMap[A, Snapshot]] {
    def empty: SortedMap[A, Snapshot] = SortedMap.empty[A, Snapshot]

    def combine(f1: SortedMap[A, Snapshot], f2: SortedMap[A, Snapshot]): SortedMap[A, Snapshot] = f1 ++ f2
  }

  implicit val blockDiffMonoid = new Monoid[BlockDiff] {
    override def empty: BlockDiff = BlockDiff.empty

    override def combine(older: BlockDiff, newer: BlockDiff): BlockDiff = BlockDiff(
      txsDiff = older.txsDiff.combine(newer.txsDiff),
      heightDiff = older.heightDiff + newer.heightDiff,
      snapshots = older.snapshots.combine(newer.snapshots))
  }
}
package com.wavesplatform.state2

import cats.Monoid
import cats.implicits._
import scorex.account.Address

import scala.collection.SortedMap

case class BlockDiff(txsDiff: Diff,
                     heightDiff: Int,
                     snapshots: Map[Address, SortedMap[Int, Snapshot]]){
  override def toString: String = s"(Δh=$heightDiff, Δtxs=${txsDiff.transactions.size})"

}

object BlockDiff {

  val empty: BlockDiff = BlockDiff(Monoid[Diff].empty, 0, Map.empty)

  implicit def sortedMapForSnapshotsMonoid[A: Ordering, S]: Monoid[SortedMap[A, S]] = new Monoid[SortedMap[A, S]] {
    def empty: SortedMap[A, S] = SortedMap.empty[A, S]

    def combine(f1: SortedMap[A, S], f2: SortedMap[A, S]): SortedMap[A, S] = f1 ++ f2
  }

  implicit val blockDiffMonoid = new Monoid[BlockDiff] {
    override def empty: BlockDiff = BlockDiff.empty

    override def combine(older: BlockDiff, newer: BlockDiff): BlockDiff = BlockDiff(
      txsDiff = older.txsDiff.combine(newer.txsDiff),
      heightDiff = older.heightDiff + newer.heightDiff,
      snapshots = older.snapshots.combine(newer.snapshots))
  }

}
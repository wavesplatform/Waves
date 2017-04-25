package com.wavesplatform.state2

import cats._
import cats.implicits._
import cats.Monoid
import scorex.account.Account

import scala.collection.SortedMap

case class BlockDiff(txsDiff: Diff,
                     heightDiff: Int,
                     updates: Map[Account, SortedMap[Int, Snapshot]])

object BlockDiff {

  implicit def sortedMapMonoid[A: scala.Ordering, B: Semigroup]: Monoid[SortedMap[A, B]] = new Monoid[SortedMap[A, B]] {
    def empty: SortedMap[A, B] = SortedMap.empty[A, B]

    def combine(f1: SortedMap[A, B], f2: SortedMap[A, B]): SortedMap[A, B] =
      Monoid[Map[A, B]].combine(f1.asInstanceOf[Map[A, B]], f2.asInstanceOf[Map[A, B]]).asInstanceOf[SortedMap[A, B]]
  }

  implicit val blockDiffMonoid = new Monoid[BlockDiff] {
    override def empty: BlockDiff = BlockDiff(Monoid[Diff].empty, 0, Map.empty)

    override def combine(older: BlockDiff, newer: BlockDiff): BlockDiff = BlockDiff(
      txsDiff = older.txsDiff.combine(newer.txsDiff),
      heightDiff = older.heightDiff + newer.heightDiff,
      updates = newer.updates.combine(older.updates))
  }
}
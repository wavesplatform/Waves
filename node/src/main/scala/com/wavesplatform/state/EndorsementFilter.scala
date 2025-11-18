package com.wavesplatform.state

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.crypto.bls.BlsPublicKey
import com.wavesplatform.state.EndorsementFilter.SimulationResult
import com.wavesplatform.state.Height

import scala.collection.mutable

case class EndorsementFilter(
    miner: Option[GeneratorIndex],
    finalizedId: BlockId,
    finalizedHeight: Height,
    endorsedId: BlockId,
    endorsers: IndexedSeq[(BlsPublicKey, Long)],
    conflict: Set[GeneratorIndex]
) {
  private val minerBalance        = miner.fold(0L)(i => endorsers(i.toInt)._2)
  private val doubledTotalBalance = endorsers.foldLeft(BigInt(0L)) { case (r, (_, b)) => r + b } * 2

  override def toString: String =
    s"EndorsementFilter(${miner.fold("")(i => s"m=$i, ")}fid=$finalizedId, fh=$finalizedHeight, eid=$endorsedId, e={${endorsers.mkString(", ")}})"

  def sameVoting(other: EndorsementFilter): Boolean =
    finalizedId == other.finalizedId && finalizedHeight == other.finalizedHeight && endorsedId == other.endorsedId

  def simulate(validIndexes: Iterable[Int], newConflictIndexes: Set[Int]): SimulationResult = {
    type Item = (idx: GeneratorIndex, blsPk: BlsPublicKey, balance: Long)
    val lifted = endorsers.lift
    val items = for {
      i                <- validIndexes.view
      (blsPk, balance) <- lifted(i)

      gi = GeneratorIndex(i)
      if !(conflict.contains(gi) || newConflictIndexes.contains(i))
    } yield (GeneratorIndex(i), blsPk, balance): Item

    val richest = mutable.PriorityQueue.empty[Item](using Ordering.by(-_.balance))
    richest.addAll(items)

    var endorserIndexes = Vector.empty[GeneratorIndex]
    var endorsedBalance = BigInt(minerBalance)
    var reached         = false
    while (richest.nonEmpty && !reached) {
      val x = richest.dequeue()
      endorserIndexes = endorserIndexes.appended(x.idx)
      endorsedBalance += x.balance
      reached = endorsedBalance * 3 >= doubledTotalBalance // Same as endorsedBalance >= totalBalance * 2 / 3, but with precision
    }

    SimulationResult(reached, endorserIndexes)
  }
}

object EndorsementFilter {
  case class SimulationResult(reachedFinalization: Boolean = false, chosenValid: IndexedSeq[GeneratorIndex] = Vector.empty)
}

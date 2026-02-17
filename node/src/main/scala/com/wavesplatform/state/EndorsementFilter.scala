package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.crypto.bls.BlsPublicKey
import com.wavesplatform.state.EndorsementFilter.SimulationResult
import com.wavesplatform.state.Height

import scala.collection.mutable

/** @param normalizedGeneratorSet All, including conflict. Zero balance means it is not enough for mining and endorsing
  */
case class EndorsementFilter(
    maxValidEndorsers: Int,
    miner: GeneratorIndex,
    isMiner: Boolean,
    finalizedId: BlockId,
    finalizedHeight: Height,
    endorsedId: BlockId,
    normalizedGeneratorSet: IndexedSeq[(address: Address, blsPk: BlsPublicKey, generatingBalance: Long)],
    conflict: Set[GeneratorIndex]
) {
  private val minerBalance = normalizedGeneratorSet.lift(miner.toInt).fold(0L)(_.generatingBalance)
  private val totalBalance = normalizedGeneratorSet.foldLeft(BigInt(0L)) { case (r, (_, _, b)) => r + b } -
    conflict.view.map(i => normalizedGeneratorSet(i.toInt).generatingBalance).sum

  override def toString: String = {
    val endorsersStr = normalizedGeneratorSet.view.map { case (addr, _, b) => s"$addr -> $b" }.mkString(", ")
    s"EndorsementFilter(m=$miner, fid=$finalizedId, fh=$finalizedHeight, eid=$endorsedId, e={$endorsersStr})"
  }

  def sameVoting(other: EndorsementFilter): Boolean =
    finalizedId == other.finalizedId && finalizedHeight == other.finalizedHeight && endorsedId == other.endorsedId

  def simulate(validIndexes: Iterable[Int], newConflictIndexes: Set[Int]): SimulationResult = {
    type Item = (idx: GeneratorIndex, blsPk: BlsPublicKey, balance: Long)
    val lifted = normalizedGeneratorSet.lift
    val items = for {
      i                   <- validIndexes.view
      (_, blsPk, balance) <- lifted(i)
      if balance > 0

      gi = GeneratorIndex(i)
      if !(gi == miner || conflict.contains(gi) || newConflictIndexes.contains(i)) // Miner is included below, ignore conflicting
    } yield (GeneratorIndex(i), blsPk, balance): Item

    val totalBalanceWithoutNewConflict = totalBalance - newConflictIndexes.view.map(normalizedGeneratorSet(_).generatingBalance).sum
    val doubledTotalBalance            = totalBalanceWithoutNewConflict * 2

    val richest         = mutable.PriorityQueue.from(items)(using Ordering.by[Item, Long](_.balance))
    var endorserIndexes = Vector.empty[GeneratorIndex]
    var endorsedBalance = BigInt(minerBalance)
    var reached         = false
    while (endorserIndexes.size < maxValidEndorsers && richest.nonEmpty && !reached) {
      val x = richest.dequeue()
      endorserIndexes = endorserIndexes.appended(x.idx)
      endorsedBalance += x.balance
      reached = endorsedBalance * 3 >= doubledTotalBalance // Same as endorsedBalance >= totalBalance * 2 / 3, but with precision
    }

    SimulationResult(reached, endorsedBalance, totalBalance, endorserIndexes)
  }
}

object EndorsementFilter {
  case class SimulationResult(
      reachedFinalization: Boolean = false,
      endorsedBalance: BigInt,
      totalBalance: BigInt,
      chosenValid: Seq[GeneratorIndex] = Nil
  )
}

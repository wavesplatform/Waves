package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, FinalizationVoting}
import com.wavesplatform.utils.ScorexLogging

case class FinalizationState(
    miner: Address,
    generatorSet: GeneratorSet = Seq.empty,
    conflictGenerators: Set[GeneratorIndex] = Set.empty,
    parentHeight: Height = GenesisBlockHeight,
    baseFinalizedHeight: Height = GenesisBlockHeight,
    accFinalizationVoting: Option[FinalizationVoting] = None,
    finalizedHeight: Height = GenesisBlockHeight,
    parentFinalized: Boolean = false
) {
  def append(
      newBlockId: BlockId,
      newFinalizationVoting: Option[FinalizationVoting],
      updatedGeneratorSet: GeneratorSet
  ): (updatedState: FinalizationState, accVoting: Option[FinalizationVoting], height: Height) = {
    val newConflictGenerators = newFinalizationVoting.fold(Nil)(_.conflict.map(_.endorserIndex)).toSet
    val (updatedParentFinalized, updatedFinalizedHeight) = newFinalizationVoting
      .filterNot(parentFinalized && _.conflict.isEmpty)
      .fold((parentFinalized, finalizedHeight)) { _ =>
        val updatedParentFinalized = FinalizationState.isParentFinalized(
          newBlockId,
          updatedGeneratorSet,
          newConflictGenerators,
          miner,
          newFinalizationVoting,
          parentHeight,
          parentFinalized
        )

        (
          updatedParentFinalized,
          if (updatedParentFinalized) parentHeight else finalizedHeight
        )
      }

    val blockFv = FinalizationVoting.combine(accFinalizationVoting, newFinalizationVoting)
    val updated = copy(
      generatorSet = updatedGeneratorSet,
      accFinalizationVoting = blockFv,
      finalizedHeight = updatedFinalizedHeight,
      parentFinalized = updatedParentFinalized,
      conflictGenerators = conflictGenerators ++ newConflictGenerators
    )

    (updated, blockFv, updatedFinalizedHeight)
  }
}

object FinalizationState extends ScorexLogging {
  def notActivated(base: Block): FinalizationState = notActivated(base.header.generator.toAddress)
  def notActivated(miner: Address): FinalizationState = new FinalizationState(
    miner,
    generatorSet = Nil,
    conflictGenerators = Set.empty,
    parentHeight = GenesisBlockHeight,
    baseFinalizedHeight = GenesisBlockHeight,
    accFinalizationVoting = None,
    finalizedHeight = GenesisBlockHeight,
    parentFinalized = false
  )

  def init(
      generatorSet: GeneratorSet,
      conflictGenerators: Set[GeneratorIndex],
      base: Block,
      parentHeight: Height = GenesisBlockHeight,
      finalizedHeight: Height = GenesisBlockHeight
  ): FinalizationState = {
    val miner           = base.header.generator.toAddress
    val baseFv          = base.header.finalizationVoting
    val parentFinalized = isParentFinalized(base.id(), generatorSet, conflictGenerators, miner, baseFv, parentHeight, finalizedPreviously = false)
    val updatedFinalizedHeight = if (parentFinalized) parentHeight else finalizedHeight
    FinalizationState(
      miner,
      generatorSet,
      conflictGenerators,
      parentHeight,
      baseFinalizedHeight = updatedFinalizedHeight,
      accFinalizationVoting = baseFv,
      updatedFinalizedHeight,
      parentFinalized
    )
  }

  // TODO: add already known as conflict, or better: generator balances without conflict
  // TODO: easier to create lambda?
  private def isParentFinalized(
      after: BlockId,
      generatorSet: GeneratorSet,
      knownConflict: Set[GeneratorIndex],
      votingBlockMinerAddress: Address,
      voting: Option[FinalizationVoting],
      parentHeight: Height,
      finalizedPreviously: Boolean
  ): Boolean = generatorSet.nonEmpty && {
    val votedIndexes       = voting.fold(Seq.empty)(_.valid)
    val votedIndexesSet    = votedIndexes.toSet
    val allConflictIndexes = knownConflict ++ voting.fold(Set.empty)(_.conflict.view.map(_.endorserIndex))
    val (totalBalance, endorsedBalance, minerIdx) = generatorSet.foldLeft((BigInt(0), BigInt(0), -1)) {
      case (orig @ (totalBalance, endorsedBalance, minerIdx), x) =>
        val gi = x.index
        if (allConflictIndexes.contains(gi)) orig
        else {
          val isMiner    = x.address == votingBlockMinerAddress
          val isEndorser = votedIndexesSet.contains(gi)
          (
            totalBalance + x.balance,
            if (isEndorser || isMiner) endorsedBalance + x.balance else endorsedBalance,
            if (isMiner) x.index.toInt else minerIdx
          )
        }
    }

    for {
      c <- voting.fold(Seq.empty)(_.conflict)
      idx = c.endorserIndex.toInt
      if 0 <= idx && idx < generatorSet.size
      x = generatorSet(idx)
    } log.debug(s"New conflict endorser ${x.address} with index $idx and balance ${x.balance}")

    val r         = FinalizationVoting.isFinalized(endorsedBalance, totalBalance)
    val statusStr = if (finalizedPreviously && !r) "Lost" else if (r) "Reached" else "Not reached"
    log.debug(
      s"$statusStr after ${after.trim} for $parentHeight, endorsed=$endorsedBalance, total=$totalBalance, " +
        s"miner=$minerIdx" +
        (if (votedIndexes.isEmpty) "" else s", valid=[${votedIndexes.mkString(", ")}]") +
        (if (allConflictIndexes.isEmpty) "" else s", conflict=[${allConflictIndexes.mkString(", ")}]")
    )

    r
  }
}

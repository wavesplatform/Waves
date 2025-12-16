package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, FinalizationVoting}
import com.wavesplatform.utils.ScorexLogging

case class FinalizationState(
    generatorBalances: GeneratorBalances = Seq.empty,
    conflictGenerators: Set[GeneratorIndex] = Set.empty, // TODO: hide in lambda?
    parentHeight: Height = GenesisBlockHeight,
    finalizedHeight: Height = GenesisBlockHeight,
    finalizationVoting: Map[BlockId, FinalizationVoting] = Map.empty,
    parentFinalized: Boolean = false
) {
  def append(
      baseGenerator: Address,
      totalBlockId: BlockId,
      totalFinalizationVoting: Option[FinalizationVoting],
      updatedBalances: GeneratorBalances
  ): FinalizationState = {
    val newConflictGenerators = conflictGenerators ++ totalFinalizationVoting.fold(Set.empty)(_.conflict.map(_.endorserIndex))
    val (updatedParentFinalized, updatedFinalizedHeight) = totalFinalizationVoting
      .filterNot(v => parentFinalized && v.conflict.isEmpty)
      .fold((parentFinalized, finalizedHeight)) { v =>
        val updatedParentFinalized = FinalizationState.isFinalized(updatedBalances, newConflictGenerators, baseGenerator, v)
        (
          updatedParentFinalized,
          if (updatedParentFinalized) parentHeight else finalizedHeight
        )
      }

    copy(
      generatorBalances = updatedBalances,
      finalizationVoting = totalFinalizationVoting.foldLeft(finalizationVoting)(_.updated(totalBlockId, _)),
      finalizedHeight = updatedFinalizedHeight,
      parentFinalized = updatedParentFinalized,
      conflictGenerators = newConflictGenerators
    )
  }
}

object FinalizationState extends ScorexLogging {
  def init(
      generatorBalances: GeneratorBalances,
      conflictGenerators: Set[GeneratorIndex],
      base: Block,
      parentHeight: Height = GenesisBlockHeight,
      finalizedHeight: Height = GenesisBlockHeight
  ): FinalizationState = {
    val v               = base.header.finalizationVoting
    val parentFinalized = v.fold(false)(isFinalized(generatorBalances, conflictGenerators, base.header.generator.toAddress, _))
    FinalizationState(
      generatorBalances,
      conflictGenerators,
      parentHeight,
      finalizedHeight = if (parentFinalized) parentHeight else finalizedHeight,
      finalizationVoting = v.fold(Map.empty)(v => Map(base.id() -> v)),
      parentFinalized
    )
  }

  // TODO: add already known as conflict, or better: generator balances without conflict
  // TODO: easier to create lambda?
  def isFinalized(
      generatorBalances: GeneratorBalances,
      knownConflict: Set[GeneratorIndex],
      votingBlockMinerAddress: Address,
      voting: FinalizationVoting
  ): Boolean = {
    val votedIndexes       = voting.valid.toSet
    val allConflictIndexes = knownConflict ++ voting.conflict.view.map(_.endorserIndex)
    val (totalBalance, endorsedBalance, minerIdx) = generatorBalances.foldLeft((BigInt(0), BigInt(0), -1)) {
      case (orig @ (totalBalance, endorsedBalance, minerIdx), x) =>
        val gi = x.index
        if (allConflictIndexes.contains(gi)) orig
        else {
          val isMiner    = x.address == votingBlockMinerAddress
          val isEndorser = votedIndexes.contains(gi)
          (
            totalBalance + x.balance,
            if (isEndorser || isMiner) endorsedBalance + x.balance else endorsedBalance,
            if (isMiner) x.index.toInt else minerIdx
          )
        }
    }

    for {
      c <- voting.conflict
      idx = c.endorserIndex.toInt
      if 0 <= idx && idx < generatorBalances.size
      x = generatorBalances(idx)
    } log.debug(s"New conflict endorser ${x.address} with index $idx and balance ${x.balance}")

    val r = FinalizationVoting.isFinalized(endorsedBalance, totalBalance)
    log.debug(
      s"${if (r) "Reached" else "Not reached"}, endorsed=$endorsedBalance, total=$totalBalance, " +
        s"miner=$minerIdx" +
        (if (voting.valid.isEmpty) "" else s", valid=[${voting.valid.view.mkString(", ")}]") +
        (if (allConflictIndexes.isEmpty) "" else s", conflict=[${allConflictIndexes.mkString(", ")}]")
    )

    r
  }
}

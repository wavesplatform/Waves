package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, FinalizationVoting}
import com.wavesplatform.utils.ScorexLogging

case class FinalizationState(
    generatorBalances: GeneratorBalances = Seq.empty,
    conflictGenerators: Set[GeneratorIndex] = Set.empty,
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
      .filterNot(parentFinalized && _.conflict.isEmpty)
      .fold((parentFinalized, finalizedHeight)) { _ =>
        val updatedParentFinalized =
          FinalizationState.isParentFinalized(updatedBalances, newConflictGenerators, baseGenerator, totalFinalizationVoting, parentHeight)
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
    val parentFinalized = isParentFinalized(generatorBalances, conflictGenerators, base.header.generator.toAddress, v, parentHeight)
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
  private def isParentFinalized(
      generatorBalances: GeneratorBalances,
      knownConflict: Set[GeneratorIndex],
      votingBlockMinerAddress: Address,
      voting: Option[FinalizationVoting],
      parentHeight: Height
  ): Boolean = generatorBalances.nonEmpty && {
    val votedIndexes       = voting.fold(Seq.empty)(_.valid)
    val votedIndexesSet    = votedIndexes.toSet
    val allConflictIndexes = knownConflict ++ voting.fold(Set.empty)(_.conflict.view.map(_.endorserIndex))
    val (totalBalance, endorsedBalance, minerIdx) = generatorBalances.foldLeft((BigInt(0), BigInt(0), -1)) {
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
      if 0 <= idx && idx < generatorBalances.size
      x = generatorBalances(idx)
    } log.debug(s"New conflict endorser ${x.address} with index $idx and balance ${x.balance}")

    val r = FinalizationVoting.isFinalized(endorsedBalance, totalBalance)
    log.debug(
      s"${if (r) "Reached" else "Not reached"} for $parentHeight, endorsed=$endorsedBalance, total=$totalBalance, " +
        s"miner=$minerIdx" +
        (if (votedIndexes.isEmpty) "" else s", valid=[${votedIndexes.mkString(", ")}]") +
        (if (allConflictIndexes.isEmpty) "" else s", conflict=[${allConflictIndexes.mkString(", ")}]")
    )

    r
  }
}

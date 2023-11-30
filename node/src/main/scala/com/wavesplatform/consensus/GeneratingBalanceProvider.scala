package com.wavesplatform.consensus

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.Blockchain

object GeneratingBalanceProvider {
  private val MinimalEffectiveBalanceForGenerator1: Long = 1000000000000L
  private val MinimalEffectiveBalanceForGenerator2: Long = 100000000000L
  private val FirstDepth                                 = 50
  private val SecondDepth                                = 1000

  def isMiningAllowed(blockchain: Blockchain, height: Int, effectiveBalance: Long): Boolean = {
    val activated = blockchain.activatedFeatures.get(BlockchainFeatures.SmallerMinimalGeneratingBalance.id).exists(height >= _)
    (!activated && effectiveBalance >= MinimalEffectiveBalanceForGenerator1) || (activated && effectiveBalance >= MinimalEffectiveBalanceForGenerator2)
  }

  // noinspection ScalaStyle
  def isEffectiveBalanceValid(blockchain: Blockchain, height: Int, block: Block, effectiveBalance: Long): Boolean =
    block.header.timestamp < blockchain.settings.functionalitySettings.minimalGeneratingBalanceAfter || (block.header.timestamp >= blockchain.settings.functionalitySettings.minimalGeneratingBalanceAfter && effectiveBalance >= MinimalEffectiveBalanceForGenerator1) ||
      blockchain.activatedFeatures
        .get(BlockchainFeatures.SmallerMinimalGeneratingBalance.id)
        .exists(height >= _) && effectiveBalance >= MinimalEffectiveBalanceForGenerator2

  def balance(blockchain: Blockchain, account: Address, blockId: Option[BlockId] = None): Long = {
    val height = blockId.flatMap(blockchain.heightOf).getOrElse(blockchain.height)
    val depth  = if (height >= blockchain.settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight) SecondDepth else FirstDepth

    val maybeChallengedMiner = blockchain.blockHeader(height + 1).flatMap(_.header.challengedHeader).map(_.generator.toAddress)
    blockchain.effectiveBalance(account, depth, blockId) + maybeChallengedMiner.map(blockchain.effectiveBalance(_, depth, blockId)).getOrElse(0L)
  }
}

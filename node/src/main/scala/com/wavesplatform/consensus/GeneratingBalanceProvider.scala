package com.wavesplatform.consensus

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.{Blockchain, Height}

object GeneratingBalanceProvider {
  val MinimalEffectiveBalanceForGenerator1: Long = 1000000000000L
  val MinimalEffectiveBalanceForGenerator2: Long = 100000000000L

  private val FirstDepth  = 50
  private val SecondDepth = 1000

  def minMiningBalance(blockchain: Blockchain, height: Height): Long = {
    val activated = blockchain.activatedFeatures.get(BlockchainFeatures.SmallerMinimalGeneratingBalance.id).exists(height >= _)
    if (activated) MinimalEffectiveBalanceForGenerator2
    else MinimalEffectiveBalanceForGenerator1
  }

  def isMiningAllowed(blockchain: Blockchain, height: Height, generatingBalance: Long): Boolean =
    generatingBalance >= MinimalEffectiveBalanceForGenerator1
      || blockchain.activatedFeatures
        .get(BlockchainFeatures.SmallerMinimalGeneratingBalance.id)
        .exists(height >= _) && generatingBalance >= MinimalEffectiveBalanceForGenerator2

  def isGeneratingBalanceValid(blockchain: Blockchain, height: Height, timestampMs: Long, balance: Long): Boolean =
    timestampMs < blockchain.settings.functionalitySettings.minimalGeneratingBalanceAfter
      || isMiningAllowed(blockchain, height, balance)

  def balance(blockchain: Blockchain, account: Address, blockId: Option[BlockId] = None): Long = {
    val height = blockId.flatMap(blockchain.heightOf).getOrElse(blockchain.height)
    val depth  = if (height >= blockchain.settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight) SecondDepth else FirstDepth

    val maybeChallengedMiner = blockchain.blockHeader(height + 1).flatMap(_.header.challengedHeader).map(_.generator.toAddress)
    blockchain.effectiveBalance(account, depth, blockId) + maybeChallengedMiner.map(blockchain.effectiveBalance(_, depth, blockId)).getOrElse(0L)
  }
}

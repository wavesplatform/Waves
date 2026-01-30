package com.wavesplatform.consensus

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.{Blockchain, Height}

object GeneratingBalanceProvider {
  val MinimalEffectiveBalanceForGenerator1: Long = 1000000000000L
  val MinimalEffectiveBalanceForGenerator2: Long = 100000000000L

  private val FirstDepth  = 50
  private val SecondDepth = 1000

  def isMiningAllowed(blockchain: Blockchain, height: Height, effectiveBalance: Long): Boolean =
    effectiveBalance >= MinimalEffectiveBalanceForGenerator1
      || blockchain.activatedFeatures
        .get(BlockchainFeatures.SmallerMinimalGeneratingBalance.id)
        .exists(height >= _) && effectiveBalance >= MinimalEffectiveBalanceForGenerator2

  def isGeneratingBalanceValid(blockchain: Blockchain, height: Height, block: Block, effectiveBalance: Long): Boolean =
    isGeneratingBalanceValid(blockchain, height, block.header.timestamp, effectiveBalance)

  def isGeneratingBalanceValid(blockchain: Blockchain, height: Height, timestampMs: Long, effectiveBalance: Long): Boolean =
    timestampMs < blockchain.settings.functionalitySettings.minimalGeneratingBalanceAfter
      || isMiningAllowed(blockchain, height, effectiveBalance)

  def balance(blockchain: Blockchain, account: Address, blockId: Option[BlockId] = None): Long = {
    val height = blockId.flatMap(blockchain.heightOf).getOrElse(blockchain.height)
    val depth  = if (height >= blockchain.settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight) SecondDepth else FirstDepth

    val maybeChallengedMiner = blockchain.blockHeader(height + 1).flatMap(_.header.challengedHeader).map(_.generator.toAddress)
    blockchain.effectiveBalance(account, depth, blockId) + maybeChallengedMiner.map(blockchain.effectiveBalance(_, depth, blockId)).getOrElse(0L)
  }
}

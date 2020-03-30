package com.wavesplatform.consensus

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider.FeatureProviderExt
import com.wavesplatform.state.Blockchain

object GeneratingBalanceProvider {
  private val MinimalEffectiveBalanceForGenerator1: Long = 1000000000000L
  private val MinimalEffectiveBalanceForGenerator2: Long = 100000000000L
  private val FirstDepth                                 = 50
  private val SecondDepth                                = 1000

  def isMiningAllowed(blockchain: Blockchain, height: Int, effectiveBalance: Long): Boolean = {
    val activated = blockchain.isFeatureActivated(BlockchainFeatures.SmallerMinimalGeneratingBalance, height)
    (!activated && effectiveBalance >= MinimalEffectiveBalanceForGenerator1) || (activated && effectiveBalance >= MinimalEffectiveBalanceForGenerator2)
  }

  def isEffectiveBalanceValid(blockchain: Blockchain, height: Int, block: Block, effectiveBalance: Long): Boolean =
    block.header.timestamp < blockchain.settings.functionalitySettings.minimalGeneratingBalanceAfter ||
      (block.header.timestamp >= blockchain.settings.functionalitySettings.minimalGeneratingBalanceAfter && effectiveBalance >= MinimalEffectiveBalanceForGenerator1) ||
      blockchain.isFeatureActivated(BlockchainFeatures.SmallerMinimalGeneratingBalance, height) && effectiveBalance >= MinimalEffectiveBalanceForGenerator2

  def balance(blockchain: Blockchain, account: Address, height: Option[Int] = None): Long = {
    val actualHeight = height.getOrElse(blockchain.height)
    val depth =
      if (actualHeight >= blockchain.settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight) SecondDepth else FirstDepth
    blockchain.effectiveBalance(account, depth, actualHeight)
  }
}

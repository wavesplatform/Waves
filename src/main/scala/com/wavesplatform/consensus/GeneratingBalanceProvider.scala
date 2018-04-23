package com.wavesplatform.consensus

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.Blockchain
import scorex.account.Address

object GeneratingBalanceProvider {
  private val MinimalEffectiveBalanceForGenerator1: Long = 1000000000000L
  private val MinimalEffectiveBalanceForGenerator2: Long = 100000000000L
  private val FirstDepth                                 = 50
  private val SecondDepth                                = 1000

  def validateTime(fs: FunctionalitySettings, timestamp: Long, balance: Long): Boolean =
    timestamp < fs.minimalGeneratingBalanceAfter || (timestamp >= fs.minimalGeneratingBalanceAfter && balance >= MinimalEffectiveBalanceForGenerator1)

  def validateHeight(blockchain: Blockchain, height: Int, balance: Long): Boolean =
    blockchain.activatedFeatures
      .get(BlockchainFeatures.SmallerMinimalGeneratingBalance.id)
      .exists(height >= _) && balance >= MinimalEffectiveBalanceForGenerator2

  def balance(blockchain: Blockchain, fs: FunctionalitySettings, height: Int, account: Address): Long = {
    val depth = if (height >= fs.generationBalanceDepthFrom50To1000AfterHeight) SecondDepth else FirstDepth
    blockchain.effectiveBalance(account, height, depth)
  }
}

/*
 *
    private def validateEffectiveBalance(blockchain: Blockchain, fs: FunctionalitySettings, block: Block, baseHeight: Int)(
      effectiveBalance: Long): Either[String, Long] =
    Either.cond(
      block.timestamp < fs.minimalGeneratingBalanceAfter ||
        (block.timestamp >= fs.minimalGeneratingBalanceAfter && effectiveBalance >= MinimalEffectiveBalanceForGenerator1) ||
        blockchain.featureActivationHeight(BlockchainFeatures.SmallerMinimalGeneratingBalance.id).exists(baseHeight >= _)
          && effectiveBalance >= MinimalEffectiveBalanceForGenerator2,
      effectiveBalance,
      s"generator's effective balance $effectiveBalance is less that required for generation"
    )

 */

package com.wavesplatform.consensus

import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.SnapshotStateReader
import scorex.account.Address

object GeneratingBalanceProvider {
  private val MinimalEffectiveBalanceForGenerator1: Long = 1000000000000L
  private val MinimalEffectiveBalanceForGenerator2: Long = 100000000000L
  private val FirstDepth                                 = 50
  private val SecondDepth                                = 1000

  def validateTime(fs: FunctionalitySettings, timestamp: Long, balance: Long): Boolean =
    timestamp < fs.minimalGeneratingBalanceAfter || (timestamp >= fs.minimalGeneratingBalanceAfter && balance >= MinimalEffectiveBalanceForGenerator1)

  def validateHeight(fp: FeatureProvider, height: Int, balance: Long): Boolean =
    fp.featureActivationHeight(BlockchainFeatures.SmallerMinimalGeneratingBalance.id)
      .exists(height >= _) && balance >= MinimalEffectiveBalanceForGenerator2

  def balance(state: SnapshotStateReader, fs: FunctionalitySettings, height: Int, account: Address): Long = {
    val depth = if (height >= fs.generationBalanceDepthFrom50To1000AfterHeight) SecondDepth else FirstDepth
    state.effectiveBalance(account, height, depth)
  }
}

package com.wavesplatform.mining

import cats.data.NonEmptyList
import com.wavesplatform.block.Block
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.settings.MinerSettings
import com.wavesplatform.state.Blockchain

case class MiningConstraints(total: MiningConstraint, keyBlock: MiningConstraint, micro: MiningConstraint)

object MiningConstraints {
  val MaxScriptRunsInBlock        = 100
  val MaxScriptsComplexityInBlock = 1000000
  val ClassicAmountOfTxsInBlock   = 100
  val MaxTxsSizeInBytes           = 1 * 1024 * 1024 // 1 megabyte

  def apply(blockchain: Blockchain, minerSettings: Option[MinerSettings] = None): MiningConstraints = {
    val isNgEnabled           = blockchain.isFeatureActivated(BlockchainFeatures.NG)
    val isMassTransferEnabled = blockchain.isFeatureActivated(BlockchainFeatures.MassTransfer)
    val isScriptEnabled       = blockchain.isFeatureActivated(BlockchainFeatures.SmartAccounts)
    val isDAppsEnabled        = blockchain.isFeatureActivated(BlockchainFeatures.Ride4DApps)

    val total: MiningConstraint =
      if (isMassTransferEnabled) OneDimensionalMiningConstraint(MaxTxsSizeInBytes, TxEstimators.sizeInBytes, "MaxTxsSizeInBytes")
      else {
        val maxTxs = if (isNgEnabled) Block.MaxTransactionsPerBlockVer3 else ClassicAmountOfTxsInBlock
        OneDimensionalMiningConstraint(maxTxs, TxEstimators.one, "MaxTxs")
      }

    new MiningConstraints(
      total =
        if (isDAppsEnabled)
          MultiDimensionalMiningConstraint(
            NonEmptyList
              .of(OneDimensionalMiningConstraint(MaxScriptsComplexityInBlock, TxEstimators.scriptsComplexity, "MaxScriptsComplexityInBlock"), total)
          )
        else if (isScriptEnabled)
          MultiDimensionalMiningConstraint(
            NonEmptyList.of(OneDimensionalMiningConstraint(MaxScriptRunsInBlock, TxEstimators.scriptRunNumber, "MaxScriptRunsInBlock"), total)
          )
        else total,
      keyBlock =
        if (isNgEnabled) OneDimensionalMiningConstraint(0, TxEstimators.one, "MaxTxsInKeyBlock")
        else OneDimensionalMiningConstraint(ClassicAmountOfTxsInBlock, TxEstimators.one, "MaxTxsInKeyBlock"),
      micro =
        if (isNgEnabled && minerSettings.isDefined)
          OneDimensionalMiningConstraint(minerSettings.get.maxTransactionsInMicroBlock, TxEstimators.one, "MaxTxsInMicroBlock")
        else MiningConstraint.Unlimited
    )
  }
}

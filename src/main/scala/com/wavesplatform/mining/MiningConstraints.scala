package com.wavesplatform.mining

import cats.data.NonEmptyList
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.settings.MinerSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.block.Block

case class MiningConstraints(total: MiningConstraint, keyBlock: MiningConstraint, micro: MiningConstraint)

object MiningConstraints {
  val MaxScriptRunsInBlock              = 100
  private val ClassicAmountOfTxsInBlock = 100
  private val MaxTxsSizeInBytes         = 1 * 1024 * 1024 // 1 megabyte

  def apply(blockchain: Blockchain, height: Int, minerSettings: Option[MinerSettings] = None): MiningConstraints = {
    val activatedFeatures     = blockchain.activatedFeaturesAt(height)
    val isNgEnabled           = activatedFeatures.contains(BlockchainFeatures.NG.id)
    val isMassTransferEnabled = activatedFeatures.contains(BlockchainFeatures.MassTransfer.id)
    val isScriptEnabled       = activatedFeatures.contains(BlockchainFeatures.SmartAccounts.id)

    val total: MiningConstraint =
      if (isMassTransferEnabled) OneDimensionalMiningConstraint(MaxTxsSizeInBytes, TxEstimators.sizeInBytes)
      else {
        val maxTxs = if (isNgEnabled) Block.MaxTransactionsPerBlockVer3 else ClassicAmountOfTxsInBlock
        OneDimensionalMiningConstraint(maxTxs, TxEstimators.one)
      }

    new MiningConstraints(
      total =
        if (isScriptEnabled)
          MultiDimensionalMiningConstraint(NonEmptyList.of(OneDimensionalMiningConstraint(MaxScriptRunsInBlock, TxEstimators.scriptRunNumber), total))
        else total,
      keyBlock =
        if (isNgEnabled)
          if (isMassTransferEnabled)
            OneDimensionalMiningConstraint(0, TxEstimators.one)
          else
            minerSettings
              .map(ms => OneDimensionalMiningConstraint(ms.maxTransactionsInKeyBlock, TxEstimators.one))
              .getOrElse(MiningConstraint.Unlimited)
        else OneDimensionalMiningConstraint(ClassicAmountOfTxsInBlock, TxEstimators.one),
      micro =
        if (isNgEnabled && minerSettings.isDefined)
          OneDimensionalMiningConstraint(minerSettings.get.maxTransactionsInMicroBlock, TxEstimators.one)
        else MiningConstraint.Unlimited
    )
  }
}

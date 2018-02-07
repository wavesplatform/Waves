package com.wavesplatform.mining

import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.settings.MinerSettings
import scorex.block.Block
import scorex.transaction.Transaction

trait SpaceEstimator {
  def max: Long
  implicit def estimate(x: Block): Long
  implicit def estimate(x: Transaction): Long
}

case class TxNumberSpaceEstimator(max: Long) extends SpaceEstimator {
  override implicit def estimate(x: Block): Long = x.transactionCount
  override implicit def estimate(x: Transaction): Long = 1
}

/**
  * @param max in bytes
  */
case class SizeSpaceEstimator(max: Long) extends SpaceEstimator {
  override implicit def estimate(x: Block): Long = x.transactionData.view.map(estimate).sum
  override implicit def estimate(x: Transaction): Long = x.bytes().length // + headers
}

case class MiningEstimators(total: SpaceEstimator, keyBlock: SpaceEstimator, micro: SpaceEstimator)

object MiningEstimators {
  private val ClassicAmountOfTxsInBlock = 100
  private val MaxTxsSizeInBytes = 1 * 1024 * 1024 // 1 megabyte

  def apply(minerSettings: MinerSettings, featureProvider: FeatureProvider, height: Int): MiningEstimators = {
    val activatedFeatures = featureProvider.activatedFeatures(height)
    val isNgEnabled = activatedFeatures.contains(BlockchainFeatures.NG.id)
    val isMassTransferEnabled = activatedFeatures.contains(BlockchainFeatures.MassTransfer.id)

    MiningEstimators(
      total = if (isMassTransferEnabled) SizeSpaceEstimator(MaxTxsSizeInBytes) else {
        val maxTxs = if (isNgEnabled) Block.MaxTransactionsPerBlockVer3 else ClassicAmountOfTxsInBlock
        TxNumberSpaceEstimator(maxTxs)
      },
      keyBlock = if (isMassTransferEnabled) TxNumberSpaceEstimator(0) else {
        val maxTxsForKeyBlock = if (isNgEnabled) minerSettings.maxTransactionsInKeyBlock else ClassicAmountOfTxsInBlock
        TxNumberSpaceEstimator(maxTxsForKeyBlock)
      },
      micro = TxNumberSpaceEstimator(minerSettings.maxTransactionsInMicroBlock)
    )
  }
}

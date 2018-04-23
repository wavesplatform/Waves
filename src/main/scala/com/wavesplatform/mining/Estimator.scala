package com.wavesplatform.mining

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.settings.MinerSettings
import com.wavesplatform.state.Blockchain
import scorex.block.Block
import scorex.transaction.Transaction

trait Estimator {
  def max: Long
  def estimate(x: Block): Long
  def estimate(x: Transaction): Long
}

case class TxNumberEstimator(max: Long) extends Estimator {
  override def estimate(x: Block): Long       = x.transactionCount
  override def estimate(x: Transaction): Long = 1
}

/**
  * @param max in bytes
  */
case class SizeEstimator(max: Long) extends Estimator {
  override def estimate(x: Block): Long       = x.transactionData.view.map(estimate).sum
  override def estimate(x: Transaction): Long = x.bytes().length // + headers
}

case class MiningEstimators(total: Estimator, keyBlock: Estimator, micro: Estimator)

object MiningEstimators {
  private val ClassicAmountOfTxsInBlock = 100
  private val MaxTxsSizeInBytes         = 1 * 1024 * 1024 // 1 megabyte

  def apply(minerSettings: MinerSettings, blockchain: Blockchain, height: Int): MiningEstimators = {
    val activatedFeatures     = blockchain.activatedFeaturesAt(height)
    val isNgEnabled           = activatedFeatures.contains(BlockchainFeatures.NG.id)
    val isMassTransferEnabled = activatedFeatures.contains(BlockchainFeatures.MassTransfer.id)

    MiningEstimators(
      total =
        if (isMassTransferEnabled) SizeEstimator(MaxTxsSizeInBytes)
        else {
          val maxTxs = if (isNgEnabled) Block.MaxTransactionsPerBlockVer3 else ClassicAmountOfTxsInBlock
          TxNumberEstimator(maxTxs)
        },
      keyBlock =
        if (isMassTransferEnabled) TxNumberEstimator(0)
        else {
          val maxTxsForKeyBlock = if (isNgEnabled) minerSettings.maxTransactionsInKeyBlock else ClassicAmountOfTxsInBlock
          TxNumberEstimator(maxTxsForKeyBlock)
        },
      micro = TxNumberEstimator(minerSettings.maxTransactionsInMicroBlock)
    )
  }
}

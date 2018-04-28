package com.wavesplatform.mining

import cats.data.NonEmptyList
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.settings.MinerSettings
import com.wavesplatform.state.Blockchain
import scorex.block.Block
import scorex.transaction.{Authorized, Transaction}

trait Estimator {
  def max: Long
  def estimate(x: Block): Long
  def estimate(x: Transaction): Long
}

case class TxNumberEstimator(max: Long) extends Estimator {
  override def estimate(x: Block): Long       = x.transactionCount
  override def estimate(x: Transaction): Long = 1
}

case class ScriptRunNumberEstimator(max: Long, blockchain: Blockchain) extends Estimator {
  override def estimate(x: Block): Long = x.transactionCount
  override def estimate(x: Transaction): Long = {
    val smartAccountRun = x match {
      case x: Transaction with Authorized if blockchain.accountScript(x.sender).isDefined => 1
      case _                                                                              => 0
    }

    val smartTokenRun = for {
      assetId <- x.assetFee._1
      desc    <- blockchain.assetDescription(assetId)
      _       <- desc.script
    } yield 1

    smartAccountRun + smartTokenRun.getOrElse(0)
  }
}

/**
  * @param max in bytes
  */
case class SizeEstimator(max: Long) extends Estimator {
  override def estimate(x: Block): Long       = x.transactionData.view.map(estimate).sum
  override def estimate(x: Transaction): Long = x.bytes().length // + headers
}

case class MiningEstimators(total: NonEmptyList[Estimator], keyBlock: Estimator, micro: Estimator)

object MiningEstimators {
  private val ClassicAmountOfTxsInBlock = 100
  private val MaxScriptRunsInBlock      = 100
  private val MaxTxsSizeInBytes         = 1 * 1024 * 1024 // 1 megabyte

  def apply(minerSettings: MinerSettings, blockchain: Blockchain, height: Int): MiningEstimators = {
    val activatedFeatures     = blockchain.activatedFeaturesAt(height)
    val isNgEnabled           = activatedFeatures.contains(BlockchainFeatures.NG.id)
    val isMassTransferEnabled = activatedFeatures.contains(BlockchainFeatures.MassTransfer.id)
    val isScriptEnabled       = activatedFeatures.contains(BlockchainFeatures.SmartAccounts.id)

    val total = NonEmptyList.one(
      if (isMassTransferEnabled) SizeEstimator(MaxTxsSizeInBytes)
      else {
        val maxTxs = if (isNgEnabled) Block.MaxTransactionsPerBlockVer3 else ClassicAmountOfTxsInBlock
        TxNumberEstimator(maxTxs)
      }
    )

    MiningEstimators(
      total = if (isScriptEnabled) ScriptRunNumberEstimator(MaxScriptRunsInBlock, blockchain) :: total else total,
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

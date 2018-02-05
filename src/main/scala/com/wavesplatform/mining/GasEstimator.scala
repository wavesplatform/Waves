package com.wavesplatform.mining

import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.settings.MinerSettings
import scorex.block.Block
import scorex.transaction.Transaction

trait GasEstimator {
  def max: Long
  implicit def estimate(x: Block): Long
  implicit def estimate(x: Transaction): Long
}

case class TxNumberGasEstimator(max: Long) extends GasEstimator {
  override implicit def estimate(x: Block): Long = x.transactionCount
  override implicit def estimate(x: Transaction): Long = 1
}

case class ComplexityGasEstimator(max: Long) extends GasEstimator {
  implicit def estimate(xs: Seq[Transaction]): Long = xs.view.map(estimateTx).sum
  override implicit def estimate(x: Block): Long = estimate(x.transactionData)
  override implicit def estimate(x: Transaction): Long = estimateTx(x)

  private def estimateTx(x: Transaction): Long = {
    import scorex.transaction._
    import scorex.transaction.assets._
    import scorex.transaction.assets.exchange.ExchangeTransaction
    import scorex.transaction.lease._

    x match {
      case _: BurnTransaction => 1
      case _: CreateAliasTransaction => 1
      case _: ExchangeTransaction => 3
      case _: GenesisTransaction => 1
      case _: IssueTransaction => 1
      case _: LeaseCancelTransaction => 1
      case _: LeaseTransaction => 1
      case _: PaymentTransaction => 1
      case _: ReissueTransaction => 1
      case _: TransferTransaction => 1
    }
  }
}

case class MiningEstimators(total: GasEstimator, keyBlock: GasEstimator, micro: GasEstimator)

object MiningEstimators {
  private val ClassicAmountOfTxsInBlock = 100
  private val MaxComplexity = 6000

  def apply(minerSettings: MinerSettings, featureProvider: FeatureProvider, height: Int): MiningEstimators = {
    val activatedFeatures = featureProvider.activatedFeatures(height)
    val isNgEnabled = activatedFeatures.contains(BlockchainFeatures.NG.id)
    val isMassTransferEnabled = activatedFeatures.contains(BlockchainFeatures.MassTransfer.id)

    MiningEstimators(
      total = if (isMassTransferEnabled) ComplexityGasEstimator(MaxComplexity) else {
        val maxTxs = if (isNgEnabled) Block.MaxTransactionsPerBlockVer3 else ClassicAmountOfTxsInBlock
        TxNumberGasEstimator(maxTxs)
      },
      keyBlock = if (isMassTransferEnabled) ComplexityGasEstimator(0) else {
        val maxTxsForKeyBlock = if (isNgEnabled) minerSettings.maxTransactionsInKeyBlock else ClassicAmountOfTxsInBlock
        TxNumberGasEstimator(maxTxsForKeyBlock)
      },
      micro = if (isMassTransferEnabled) ComplexityGasEstimator(minerSettings.maxComplexityInMicroBlock)
      else TxNumberGasEstimator(minerSettings.maxTransactionsInMicroBlock)
    )
  }
}

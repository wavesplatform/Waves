package com.wavesplatform.mining

import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.settings.MinerSettings
import scorex.block.Block
import scorex.transaction.Transaction

trait GasTank {
  def isEmpty: Boolean
  def -=(x: Block): Unit
  def -=(x: Transaction): Unit
  def copy(): GasTank
}

class OneGasTank private(private var restGas: Long, private val constraint: GasEstimator) extends GasTank {
  private var _isEmpty = restGas <= 0
  override def isEmpty: Boolean = _isEmpty

  override def -=(x: Block): Unit = this -= constraint.estimate(x)
  override def -=(x: Transaction): Unit = this -= constraint.estimate(x)
  private def -=(x: Long): Unit = {
    val updatedRestGas = restGas - x
    if (updatedRestGas <= 0) _isEmpty = true
    if (updatedRestGas >= 0) restGas = updatedRestGas
  }

  override def copy(): OneGasTank = new OneGasTank(restGas, constraint)
}

object OneGasTank {
  def full(constraint: GasEstimator): GasTank = new OneGasTank(constraint.max, constraint)
}

class DoubleGasTank(private val first: GasTank,
                    private val second: GasTank) extends GasTank {
  override def isEmpty: Boolean = first.isEmpty || second.isEmpty

  override def -=(x: Block): Unit = {
    first -= x
    second -= x
  }

  override def -=(x: Transaction): Unit = {
    first -= x
    second -= x
  }

  override def copy(): DoubleGasTank = new DoubleGasTank(first.copy(), second.copy())
}

object DoubleGasTank {
  def full(first: GasEstimator, second: GasEstimator): GasTank = new DoubleGasTank(
    first = OneGasTank.full(first),
    second = OneGasTank.full(second)
  )
  def partial(firstRest: GasTank, secondRest: GasTank): GasTank = new DoubleGasTank(firstRest, secondRest)
}

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
    def isNgEnabled: Boolean = featureProvider.featureActivationHeight(BlockchainFeatures.NG.id).exists(height > _ + 1)
    def isMassTransferEnabled: Boolean = featureProvider.featureActivationHeight(BlockchainFeatures.MassTransfer.id).exists(height > _ + 1)

    MiningEstimators(
      total = if (isMassTransferEnabled) ComplexityGasEstimator(MaxComplexity) else {
        val maxTxs = if (isNgEnabled) Block.MaxTransactionsPerBlockVer3 else ClassicAmountOfTxsInBlock
        TxNumberGasEstimator(maxTxs)
      },
      keyBlock = if (isMassTransferEnabled) ComplexityGasEstimator(0) else {
        val maxTxsForKeyBlock = if (isNgEnabled) minerSettings.maxTransactionsInKeyBlock else ClassicAmountOfTxsInBlock
        TxNumberGasEstimator(maxTxsForKeyBlock)
      },
      micro = if (isMassTransferEnabled) ComplexityGasEstimator(0) else TxNumberGasEstimator(minerSettings.maxTransactionsInMicroBlock)
    )
  }
}

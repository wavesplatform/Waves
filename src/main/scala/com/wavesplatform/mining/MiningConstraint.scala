package com.wavesplatform.mining

import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.settings.MinerSettings
import scorex.block.Block
import scorex.transaction.Transaction

trait MiningConstraintUpdater {
  def wasMet: Boolean
  def -=(x: Block): Unit
  def -=(x: Transaction): Unit
  def copy(): MiningConstraintUpdater
}

class OneMiningConstraintUpdater private(private var restGas: Long,
                                         private val constraint: MiningConstraint) extends MiningConstraintUpdater {
  private var _wasMet = restGas <= 0

  override def wasMet: Boolean = _wasMet
  override def -=(x: Block): Unit = this -= constraint.estimate(x)
  override def -=(x: Transaction): Unit = this -= constraint.estimate(x)

  private def -=(x: Long): Unit = {
    val updatedRestGas = restGas - x
    if (updatedRestGas <= 0) _wasMet = true
    if (updatedRestGas >= 0) restGas = updatedRestGas
  }

  override def copy(): OneMiningConstraintUpdater = new OneMiningConstraintUpdater(restGas, constraint)
}

object OneMiningConstraintUpdater {
  def full(constraint: MiningConstraint): MiningConstraintUpdater = new OneMiningConstraintUpdater(constraint.max, constraint)
}

class TwoMiningConstraintUpdater(private val first: MiningConstraintUpdater,
                                 private val second: MiningConstraintUpdater) extends MiningConstraintUpdater {
  override def wasMet: Boolean = first.wasMet || second.wasMet

  override def -=(x: Block): Unit = {
    first -= x
    second -= x
  }

  override def -=(x: Transaction): Unit = {
    first -= x
    second -= x
  }

  override def copy(): TwoMiningConstraintUpdater = new TwoMiningConstraintUpdater(first.copy(), second.copy())
}

object TwoMiningConstraintUpdater {
  def full(first: MiningConstraint, second: MiningConstraint): MiningConstraintUpdater = new TwoMiningConstraintUpdater(
    first = OneMiningConstraintUpdater.full(first),
    second = OneMiningConstraintUpdater.full(second)
  )
  def partial(firstRest: MiningConstraintUpdater, secondRest: MiningConstraintUpdater): MiningConstraintUpdater = new TwoMiningConstraintUpdater(firstRest, secondRest)
}

trait MiningConstraint {
  def max: Long
  implicit def estimate(x: Block): Long
  implicit def estimate(x: Transaction): Long
}

case class TxNumberMiningConstraint(max: Long) extends MiningConstraint {
  override implicit def estimate(x: Block): Long = x.transactionData.size
  override implicit def estimate(x: Transaction): Long = 1
}

case class ComplexityMiningConstraint(max: Long) extends MiningConstraint {
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

case class EntireMiningConstraints(total: MiningConstraint, keyBlock: MiningConstraint, micro: MiningConstraint)

object EntireMiningConstraints {
  private val ClassicAmountOfTxsInBlock: Int = 100

  def apply(minerSettings: MinerSettings, featureProvider: FeatureProvider, height: Int): EntireMiningConstraints = {
    def isNgEnabled: Boolean = featureProvider.featureActivationHeight(BlockchainFeatures.NG.id).exists(height > _ + 1)
    def isMassTransferEnabled: Boolean = featureProvider.featureActivationHeight(BlockchainFeatures.MassTransfer.id).exists(height > _ + 1)

    val total = if (isMassTransferEnabled) ComplexityMiningConstraint(0) else {
      val maxTxs = if (isNgEnabled) Block.MaxTransactionsPerBlockVer3 else ClassicAmountOfTxsInBlock
      TxNumberMiningConstraint(maxTxs)
    }

    val keyBlock = if (isMassTransferEnabled) ComplexityMiningConstraint(0) else {
      val maxTxsForKeyBlock = if (isNgEnabled) minerSettings.maxTransactionsInKeyBlock else ClassicAmountOfTxsInBlock
      TxNumberMiningConstraint(maxTxsForKeyBlock)
    }

    val micro = if (isMassTransferEnabled) ComplexityMiningConstraint(0) else TxNumberMiningConstraint(minerSettings.maxTransactionsInMicroBlock)

    EntireMiningConstraints(total, keyBlock, micro)
  }
}

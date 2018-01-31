package com.wavesplatform.mining

import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.settings.MinerSettings
import scorex.block.Block
import scorex.transaction.Transaction

import scala.reflect.ClassTag

trait MiningLimit {
  implicit def estimate(x: Block): MiningLimit
  implicit def estimate(x: Transaction): MiningLimit

  def wasMet: Boolean
  def meet: MiningLimit
  def -(x: MiningLimit): MiningLimit
}

object MiningLimit {
  def safe[Limit](x: MiningLimit)(f: Limit => Limit)(implicit ct: ClassTag[Limit]): Limit = x match {
    case that: Limit => f(that)
    case _ => throw new IllegalArgumentException(s"Can't combine two different types of mining limits: ${getClass.getName} vs ${x.getClass.getName}")
  }
}

case class CombinedMiningLimit(block: MiningLimit, micro: MiningLimit) extends MiningLimit {
  override implicit def estimate(x: Block): CombinedMiningLimit = CombinedMiningLimit(block.estimate(x), micro.estimate(x))
  override implicit def estimate(x: Transaction): CombinedMiningLimit = CombinedMiningLimit(block.estimate(x), micro.estimate(x))

  override def wasMet: Boolean = block.wasMet || micro.wasMet

  override def meet: CombinedMiningLimit = CombinedMiningLimit(block.meet, micro.meet)

  override def -(x: MiningLimit): CombinedMiningLimit = MiningLimit.safe(x) { safeX: CombinedMiningLimit =>
    CombinedMiningLimit(block - safeX.block, micro - safeX.micro)
  }
}

object CombinedMiningLimit {
  private val ClassicAmountOfTxsInBlock: Int = 100

  def total(minerSettings: MinerSettings, featureProvider: FeatureProvider, height: Int): CombinedMiningLimit = {
    def ngEnabled: Boolean = featureProvider.featureActivationHeight(BlockchainFeatures.NG.id).exists(height > _ + 1)

    val maxTxsForBlock = if (ngEnabled) minerSettings.maxTransactionsInKeyBlock else ClassicAmountOfTxsInBlock
    val maxTxsForMicro = Math.min(Block.MaxTransactionsPerBlockVer3, minerSettings.maxTransactionsInMicroBlock)
    CombinedMiningLimit(TxNumberMiningLimit(maxTxsForBlock), TxNumberMiningLimit(maxTxsForMicro))
  }
}

case class TxNumberMiningLimit(restNumber: Long, wasMet: Boolean = false) extends MiningLimit {
  override implicit def estimate(x: Block): TxNumberMiningLimit = TxNumberMiningLimit(x.transactionData.size)
  override implicit def estimate(x: Transaction): TxNumberMiningLimit = TxNumberMiningLimit(1)

  override def meet: TxNumberMiningLimit = copy(wasMet = true)

  override def -(x: MiningLimit): TxNumberMiningLimit = MiningLimit.safe(x) { safeX: TxNumberMiningLimit =>
    val updatedRestNumber = restNumber - safeX.restNumber
    if (updatedRestNumber < 0) copy(wasMet = true)
    else if (updatedRestNumber == 0) copy(restNumber = 0, wasMet = true)
    else copy(restNumber = updatedRestNumber)
  }
}

case class ComplexityMiningLimit(restValue: Long, wasMet: Boolean = false) extends MiningLimit {
  implicit def estimate(xs: Seq[Transaction]): ComplexityMiningLimit = ComplexityMiningLimit(xs.view.map(estimateTx).sum)
  override implicit def estimate(x: Block): ComplexityMiningLimit = estimate(x.transactionData)
  override implicit def estimate(x: Transaction): ComplexityMiningLimit = ComplexityMiningLimit(estimateTx(x))

  override def meet: ComplexityMiningLimit = copy(wasMet = true)

  override def -(x: MiningLimit): ComplexityMiningLimit = MiningLimit.safe(x) { safeX: ComplexityMiningLimit =>
    val updatedRestValue = this.restValue - safeX.restValue
    if (updatedRestValue < 0) copy(wasMet = true)
    else if (updatedRestValue == 0) copy(restValue = 0, wasMet = true)
    else copy(restValue = updatedRestValue)
  }

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

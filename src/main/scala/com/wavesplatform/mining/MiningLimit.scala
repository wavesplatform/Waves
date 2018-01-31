package com.wavesplatform.mining

import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.settings.MinerSettings
import scorex.block.Block
import scorex.transaction.Transaction

trait MiningLimit {
  implicit def estimate(x: Block): MiningLimit
  implicit def estimate(x: Transaction): MiningLimit

  def wasMet: Boolean
  def -(x: MiningLimit): MiningLimit
}

case class CombinedMiningLimit(block: MiningLimit, micro: MiningLimit) extends MiningLimit {
  override implicit def estimate(x: Block): CombinedMiningLimit = CombinedMiningLimit(block.estimate(x), micro.estimate(x))
  override implicit def estimate(x: Transaction): CombinedMiningLimit = CombinedMiningLimit(block.estimate(x), micro.estimate(x))

  override def wasMet: Boolean = block.wasMet && micro.wasMet

  override def -(x: MiningLimit): CombinedMiningLimit = safeOp(x) { safeX =>
    CombinedMiningLimit(block - safeX.block, micro - safeX.micro)
  }

  private def safeOp[T](x: MiningLimit)(f: CombinedMiningLimit => T): T = x match {
    case that: CombinedMiningLimit => f(that)
    case _ => throw new IllegalArgumentException(s"Can't combine two different types of mining limits: ${getClass.getName} vs ${x.getClass.getName}")
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

  override def -(x: MiningLimit): MiningLimit = safeOp(x) { safeX =>
    val updatedRestNumber = restNumber - safeX.restNumber
    if (updatedRestNumber < 0) copy(restNumber = this.restNumber, wasMet = true)
    else copy(restNumber = updatedRestNumber)
  }

  private def safeOp[T](x: MiningLimit)(f: TxNumberMiningLimit => T): T = x match {
    case that: TxNumberMiningLimit => f(that)
    case _ => throw new IllegalArgumentException(s"Can't combine two different types of mining limits: ${getClass.getName} vs ${x.getClass.getName}")
  }
}

case class ComplexityMiningLimit(value: Long, wasMet: Boolean = false) extends MiningLimit {
  override implicit def estimate(x: Block): ComplexityMiningLimit = ComplexityMiningLimit(x.transactionData.view.map(estimateTx).sum)
  override implicit def estimate(x: Transaction): ComplexityMiningLimit = ComplexityMiningLimit(estimateTx(x))

  override def -(x: MiningLimit): ComplexityMiningLimit = x match {
    case that: ComplexityMiningLimit => copy(value = value - that.value)
    case _ => throw new IllegalArgumentException(s"Can't subtract two different types of mining limits: ${getClass.getName} vs ${x.getClass.getName}")
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

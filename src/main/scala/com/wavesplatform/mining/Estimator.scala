package com.wavesplatform.mining

import com.wavesplatform.state.Blockchain
import scorex.block.Block
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.assets.{BurnTransaction, ReissueTransaction, SponsorFeeTransaction}
import scorex.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import scorex.transaction.{Authorized, Transaction}

trait Estimator {
  def estimate(blockchain: Blockchain, x: Block): Long
  def estimate(blockchain: Blockchain, x: Transaction): Long
}

object TxNumberEstimator extends Estimator {
  override def estimate(blockchain: Blockchain, x: Block): Long       = x.transactionCount
  override def estimate(blockchain: Blockchain, x: Transaction): Long = 1
}

object ScriptRunNumberEstimator extends Estimator {
  override def estimate(blockchain: Blockchain, x: Block): Long = x.transactionData.map(estimate(blockchain, _)).sum
  override def estimate(blockchain: Blockchain, x: Transaction): Long = {
    val smartAccountRun = x match {
      case x: Transaction with Authorized if blockchain.accountScript(x.sender).isDefined => 1
      case _                                                                              => 0
    }

    val assetIds = x match {
      case x: TransferTransaction     => x.assetId.toSeq
      case x: MassTransferTransaction => x.assetId.toSeq
      case x: BurnTransaction         => Seq(x.assetId)
      case x: ReissueTransaction      => Seq(x.assetId)
      case x: SponsorFeeTransaction   => Seq(x.assetId)
      case x: ExchangeTransaction     => Seq(x.buyOrder.assetPair.amountAsset, x.buyOrder.assetPair.priceAsset).flatten
      case _                          => Seq.empty
    }

    val smartTokenRuns = assetIds.flatMap(blockchain.assetDescription).count(_.script.isDefined)
    smartAccountRun + smartTokenRuns
  }
}

object SizeInBytesEstimator extends Estimator {
  override def estimate(blockchain: Blockchain, x: Block): Long       = x.transactionData.view.map(estimate(blockchain, _)).sum
  override def estimate(blockchain: Blockchain, x: Transaction): Long = x.bytes().length // + headers
}

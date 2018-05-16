package com.wavesplatform.mining

import com.wavesplatform.state.Blockchain
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.assets.{BurnTransaction, ReissueTransaction, SponsorFeeTransaction}
import scorex.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import scorex.transaction.{Authorized, Transaction}

object TxEstimators {
  def sizeInBytes(blockchain: Blockchain, x: Transaction): Long = x.bytes().length // + headers
  def one(blockchain: Blockchain, x: Transaction): Long         = 1
  def scriptRunNumber(blockchain: Blockchain, x: Transaction): Long = {
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

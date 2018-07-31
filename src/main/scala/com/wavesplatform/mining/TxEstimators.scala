package com.wavesplatform.mining

import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.assets.{BurnTransaction, ReissueTransaction, SponsorFeeTransaction}
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.transaction.{Authorized, Transaction}

object TxEstimators {
  abstract class Fn extends ((Blockchain, Transaction) => Long) {
    val minEstimate: Long
  }

  object sizeInBytes extends Fn {
    override def apply(blockchain: Blockchain, x: Transaction): Long = x.bytes().length // + headers

    override def toString(): String = "sizeInBytes"

    override val minEstimate = 109
  }

  object one extends Fn {
    override def apply(blockchain: Blockchain, x: Transaction): Long = 1

    override def toString(): String = "one"

    override val minEstimate = 1
  }

  object scriptRunNumber extends Fn {
    override def apply(blockchain: Blockchain, x: Transaction): Long = {
      val smartAccountRun = x match {
        case x: Transaction with Authorized if blockchain.hasScript(x.sender) => 1
        case _                                                                => 0
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

    override def toString(): String = "scriptRunNumber"

    override val minEstimate = 0
  }
}

package com.wavesplatform.mining

import com.wavesplatform.state.{Blockchain, Diff}
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.ScorexLogging

//noinspection ScalaStyle
object TxEstimators extends ScorexLogging {
  abstract class Fn extends ((Blockchain, Transaction, Diff) => Long) {
    val minEstimate: Long
  }

  object sizeInBytes extends Fn {
    override def apply(blockchain: Blockchain, x: Transaction, diff: Diff): Long = x.bytes().length // + headers

    override def toString(): String = "sizeInBytes"

    override val minEstimate = 109L
  }

  object one extends Fn {
    override def apply(blockchain: Blockchain, x: Transaction, diff: Diff): Long = 1

    override def toString(): String = "one"

    override val minEstimate = 1L
  }

  object scriptRunNumber extends Fn {
    override def apply(blockchain: Blockchain, x: Transaction, diff: Diff): Long = {
// begin old code for test
      import com.wavesplatform.transaction.Asset.IssuedAsset
      import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
      import com.wavesplatform.transaction.assets.{BurnTransaction, ReissueTransaction, SponsorFeeTransaction}
      import com.wavesplatform.transaction.smart.InvokeScriptTransaction
      import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
      import com.wavesplatform.transaction.{Authorized, Transaction}

      val smartAccountRun = x match {
        case x: Transaction with Authorized if blockchain.hasScript(x.sender) => 1
        case _                                                                => 0
      }

      val assetIds = x match {
        case x: TransferTransaction     => x.assetId.fold[Seq[IssuedAsset]](Nil)(Seq(_))
        case x: MassTransferTransaction => x.assetId.fold[Seq[IssuedAsset]](Nil)(Seq(_))
        case x: BurnTransaction         => Seq(x.asset)
        case x: ReissueTransaction      => Seq(x.asset)
        case x: SponsorFeeTransaction   => Seq(x.asset)
        case x: ExchangeTransaction =>
          Seq(
            x.buyOrder.assetPair.amountAsset.fold[Seq[IssuedAsset]](Nil)(Seq(_)),
            x.buyOrder.assetPair.priceAsset.fold[Seq[IssuedAsset]](Nil)(Seq(_))
          ).flatten
        case _ => Seq.empty
      }
      val smartTokenRuns = assetIds.flatMap(blockchain.assetDescription).count(_.script.isDefined)

      val invokeScriptRun = x match {
        case tx: InvokeScriptTransaction => 1
        case _                           => 0
      }

      if(diff.scriptsRun != smartAccountRun + smartTokenRuns + invokeScriptRun) {
        log.error(s"$x\n$diff\n ${diff.scriptsRun} ${smartAccountRun + smartTokenRuns + invokeScriptRun}")
      }
// end old code for test

      diff.scriptsRun
    }

    override def toString(): String = "scriptRunNumber"

    override val minEstimate = 0L
  }
}

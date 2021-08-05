package com.wavesplatform.state.diffs

import cats.syntax.either._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionDiff
import com.wavesplatform.state.{Blockchain, Diff}
import com.wavesplatform.transaction.TxValidationError.{GenericError, InvalidAssetId}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{Asset, EthereumTransaction}

object EthereumTransactionDiff {
  def apply(blockchain: Blockchain, currentBlockTs: Long)(e: EthereumTransaction): TracedResult[ValidationError, Diff] = e match {
    case et: EthereumTransaction.Transfer =>
      TracedResult(for {
        asset <- et.asset.map[Either[ValidationError, Asset]](c => blockchain.resolveERC20Address(c).toRight(InvalidAssetId)).valueOr(Right(_))
        diff  <- TransferDiff(blockchain)(et.sender, et.recipient, et.amount, asset, et.assetFee._2, et.assetFee._1)
      } yield diff)

    case et: EthereumTransaction.InvokeScript => {
      for {
        dAppAddress <- TracedResult(blockchain.resolveAlias(et.dApp))
        scriptInfo <- TracedResult(blockchain.accountScript(dAppAddress).toRight(GenericError(s"No script at address $dAppAddress")))
        diff <- InvokeScriptTransactionDiff(blockchain, currentBlockTs, limitedExecution = true)(et.toInvokable(scriptInfo.script))
      } yield diff
    }
  }
}

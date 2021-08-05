package com.wavesplatform.state.diffs

import cats.syntax.either._
import com.google.protobuf.ByteString
import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_STRING, FUNC, FUNCTION_CALL, REF}
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.state.{AccountScriptInfo, Blockchain, Diff}
import com.wavesplatform.state.diffs.invoke.{InvokeScriptDiff, InvokeScriptTransactionDiff}
import com.wavesplatform.transaction.TxValidationError.{GenericError, InvalidAssetId}
import com.wavesplatform.transaction.{ABIConverter, Asset, EthereumTransaction}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.contract.meta.{MetaMapper, V2}
import com.wavesplatform.lang.v1.compiler.Types
import com.wavesplatform.protobuf.dapp.DAppMeta.{CallableFuncSignature, CompactNameAndOriginalNamePair}

import java.lang.reflect.Modifier

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

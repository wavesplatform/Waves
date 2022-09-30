package com.wavesplatform.ride.input

import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.api.http.requests.InvokeScriptRequest
import com.wavesplatform.api.http.requests.InvokeScriptRequest.FunctionCallPart
import com.wavesplatform.api.http.utils.UtilsEvaluator
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.{StdLibVersion, V6}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionLike
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeExpressionTransaction, InvokeScriptTransaction}
import com.wavesplatform.transaction.{Asset, Proofs, TxPositiveAmount, TxTimestamp, TxVersion}

case class RunnerRequest(
    trace: Boolean,
    senderPublicKey: PublicKey,
    call: Either[RunnerExpr, RunnerCall],
    fee: TxPositiveAmount = TxPositiveAmount(1_000_000),
    feeAssetId: Asset = Waves,
    timestamp: TxTimestamp = System.currentTimeMillis(),
    proofs: Proofs = Proofs.empty
) {
  def toTx(thatChainId: Byte): InvokeScriptTransactionLike = call match {
    case Left(expr) =>
//      new InvokeScriptTransactionLike {
//        override def assetFee: (Asset, Long) = (feeAssetId, fee.value)
//        override def timestamp: TxTimestamp = timestamp
//        override def chainId: TxVersion = thatChainId
//        override def id: Coeval[ByteStr] = Coeval.now(ByteStr.empty)
//        override val tpe: TransactionType = TransactionType.InvokeScript
//        override def dApp: AddressOrAlias = senderPublicKey.toAddress(thatChainId)
//        override def funcCall: Terms.FUNCTION_CALL = ???
//
//        override def payments: Seq[Payment] = ???
//
//        override def root: InvokeScriptTransactionLike = ???
//
//        override val sender: PublicKey = _
//      }
      InvokeExpressionTransaction(
        version = TxVersion.V1,
        sender = senderPublicKey,
        expression = ExprScript(expr.stdLibVersion, expr.toExpr, isFreeCall = true, checkSize = false).explicitGet(),
        fee = fee,
        feeAssetId = feeAssetId,
        timestamp = timestamp,
        proofs = proofs,
        chainId = thatChainId
      )
    case Right(call) =>
      InvokeScriptTransaction(
        version = TxVersion.V2,
        sender = senderPublicKey,
        dApp = call.dApp,
        funcCallOpt = Some(InvokeScriptRequest.buildFunctionCall(call.call)),
        payments = call.payments,
        fee = fee,
        feeAssetId = feeAssetId,
        timestamp = timestamp,
        proofs = proofs,
        chainId = thatChainId
      )
  }
}

case class RunnerCall(
    dApp: AddressOrAlias,
    call: FunctionCallPart = FunctionCallPart("default", Nil),
    payments: Seq[Payment] = Nil
)

case class RunnerExpr(
    expr: String,
    stdLibVersion: StdLibVersion = V6
) {
  // ([], expr) is what InvokeExpressionTransaction requires
  def toExpr: EXPR = UtilsEvaluator.compile(stdLibVersion)(s"([], $expr)").explicitGet()
}

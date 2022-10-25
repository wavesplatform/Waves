package com.wavesplatform.ride.input

import com.wavesplatform.account.{Address, AddressOrAlias, PublicKey}
import com.wavesplatform.api.http.requests.InvokeScriptRequest
import com.wavesplatform.api.http.requests.InvokeScriptRequest.FunctionCallPart
import com.wavesplatform.api.http.utils.{UtilsEvaluator, UtilsInvocationRequest}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.values.{StdLibVersion, V6}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionLike
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeExpressionTransaction, InvokeScriptTransaction}
import com.wavesplatform.transaction.{Asset, Proofs, TxPositiveAmount, TxTimestamp, TxVersion}

case class RunnerRequest(
    call: Either[RunnerExpr, RunnerCall],
    trace: Boolean = false,
    sender: Option[Address] = None,
    senderPublicKey: Option[PublicKey] = None,
    feeAssetId: Asset = Waves,
    fee: TxPositiveAmount = TxPositiveAmount(5_000_000_000L), // 50 WAVES
    timestamp: TxTimestamp = System.currentTimeMillis(),
    proofs: List[String] = Nil
) {
  val senderPk: PublicKey = senderPublicKey.getOrElse(EmptyPublicKey)
  def exactSender(chainId: Byte): Address = sender.getOrElse(senderPk.toAddress(chainId))

  def toTx(chainId: Byte): InvokeScriptTransactionLike = {
    val decodedProofs = Proofs(proofs.map(decodeStringLikeBytes))
    call match {
      case Left(expr) =>
        InvokeExpressionTransaction(
          version = TxVersion.V1,
          sender = senderPk,
          expression = ExprScript(expr.stdLibVersion, expr.toExpr, isFreeCall = true, checkSize = false).explicitGet(),
          fee = fee,
          feeAssetId = feeAssetId,
          timestamp = timestamp,
          proofs = decodedProofs,
          chainId = chainId
        )
      case Right(call) =>
        InvokeScriptTransaction(
          version = TxVersion.V2,
          sender = senderPk,
          dApp = call.dApp,
          funcCallOpt = Some(InvokeScriptRequest.buildFunctionCall(call.call)),
          payments = call.payments.map(x => Payment(x.amount, x.assetId)),
          fee = fee,
          feeAssetId = feeAssetId,
          timestamp = timestamp,
          proofs = decodedProofs,
          chainId = chainId
        )
    }
  }

  def toExpr(thatChainId: Byte, script: Script): Either[ValidationError, EXPR] = {
    call match {
      case Left(expr) =>
        // TODO SerdeV1.deserialize(bytes.arr).
        UtilsEvaluator.compile(expr.stdLibVersion)(expr.expr)

      case Right(call) =>
        UtilsInvocationRequest(
          call = call.call,
          // id = ???,
          fee = fee.value,
          feeAssetId = feeAssetId.maybeBase58Repr,
          sender = Some(exactSender(thatChainId).toString),
          senderPublicKey = senderPk.toString,
          payment = call.payments.map(x => Payment(x.amount, x.assetId))
        ).toInvocation
          .flatMap(UtilsEvaluator.toExpr(script, _))
          .left
          .map(e => GenericError(e.toString))
    }
  }
}

case class RunnerCall(
    dApp: AddressOrAlias,
    call: FunctionCallPart = FunctionCallPart("default", Nil),
    payments: Seq[RunnerCallPayment] = Nil
)

case class RunnerCallPayment(amount: Long, assetId: Asset = Waves)

case class RunnerExpr(
    expr: String, // TODO Either[ByteStr, String]
    stdLibVersion: StdLibVersion = V6
) {
  // ([], expr) is what InvokeExpressionTransaction requires
  def toExpr: EXPR = UtilsEvaluator.compile(stdLibVersion)(s"([], $expr)").explicitGet()
}

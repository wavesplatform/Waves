package com.wavesplatform.api.http.utils

import cats.implicits.{toBifunctorOps, toTraverseOps}
import com.wavesplatform.account.PublicKey
import com.wavesplatform.api.http.requests.InvokeScriptRequest
import com.wavesplatform.api.http.requests.InvokeScriptRequest.FunctionCallPart
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.Invocation
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{AttachedPaymentExtractor, InvokeTransaction}
import play.api.libs.json.{Json, Reads}

case class UtilsInvocationRequest(
    call: Option[FunctionCallPart],
    id: String,
    fee: Long,
    feeAssetId: Option[String],
    senderPublicKey: String,
    payment: Seq[Payment]
) {
  def toInvocation: Either[ValidationError, Invocation] =
    for {
      senderPK   <- PublicKey.fromBase58String(senderPublicKey)
      feeAssetId <- feeAssetId.traverse(decodeBase58)
      id         <- decodeBase58(id)
      sender       = Recipient.Address(ByteStr(senderPK.toAddress.bytes))
      functionCall = call.map(InvokeScriptRequest.buildFunctionCall).getOrElse(InvokeTransaction.DefaultCall)
      payments <- AttachedPaymentExtractor
        .extractPayments(payment, V6, blockchainAllowsMultiPayment = true, smart.DApp)
        .leftMap(GenericError(_))
    } yield Invocation(functionCall, sender, senderPK, sender, senderPK, payments, id, fee, feeAssetId)

  private def decodeBase58(base58: String): Either[ValidationError, ByteStr] =
    ByteStr.decodeBase58(base58).toEither.leftMap(e => GenericError(String.valueOf(e.getMessage)))
}

object UtilsInvocationRequest {
  implicit val reads: Reads[UtilsInvocationRequest] = Json.reads[UtilsInvocationRequest]
}

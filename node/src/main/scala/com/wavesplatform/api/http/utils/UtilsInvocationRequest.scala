package com.wavesplatform.api.http.utils

import cats.implicits.{toBifunctorOps, toTraverseOps}
import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.api.http.requests.InvokeScriptRequest
import com.wavesplatform.api.http.requests.InvokeScriptRequest.FunctionCallPart
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.Invocation
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{AttachedPaymentExtractor, InvokeTransaction}
import play.api.libs.json.{Json, Reads}

case class UtilsInvocationRequest(
    call: Option[FunctionCallPart],
    id: Option[String],
    fee: Option[Long],
    feeAssetId: Option[String],
    sender: Option[String],
    senderPublicKey: Option[String],
    payment: Option[Seq[Payment]]
) {
  def toInvocation: Either[ValidationError, Invocation] =
    for {
      senderPK <- senderPublicKey.map(PublicKey.fromBase58String).getOrElse(Right(ByteStr(new Array[Byte](32))))
      id       <- id.map(decodeBase58).getOrElse(Right(ByteStr(new Array[Byte](32))))
      functionCall = call.map(InvokeScriptRequest.buildFunctionCall).getOrElse(InvokeTransaction.DefaultCall)
      feeAssetId <- feeAssetId.traverse(decodeBase58)
      sender <- sender
        .map(AddressOrAlias.fromString(_, checkChainId = false).map(a => Address(ByteStr(a.bytes))))
        .getOrElse(Right(Address(ByteStr(new Array[Byte](26)))))
      payments <- AttachedPaymentExtractor
        .extractPayments(payment.getOrElse(Nil), V6, blockchainAllowsMultiPayment = true, smart.DApp)
        .leftMap(GenericError(_))
    } yield Invocation(functionCall, sender, senderPK, sender, senderPK, payments, id, fee.getOrElse(0), feeAssetId)

  private def decodeBase58(base58: String): Either[ValidationError, ByteStr] =
    ByteStr.decodeBase58(base58).toEither.leftMap(e => GenericError(String.valueOf(e.getMessage)))
}

object UtilsInvocationRequest {
  implicit val reads: Reads[UtilsInvocationRequest] = Json.reads[UtilsInvocationRequest]
}

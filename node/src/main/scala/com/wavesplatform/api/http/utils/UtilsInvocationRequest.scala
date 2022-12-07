package com.wavesplatform.api.http.utils

import cats.implicits.{toBifunctorOps, toTraverseOps}
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.http.requests.InvokeScriptRequest
import com.wavesplatform.api.http.requests.InvokeScriptRequest.FunctionCallPart
import com.wavesplatform.api.http.utils.UtilsInvocationRequest.empty32Bytes
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.Invocation
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address as RideAddress
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.AttachedPaymentExtractor
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.{TransactionType, smart}
import play.api.libs.json.{Json, Reads}

case class UtilsInvocationRequest(
    call: FunctionCallPart = FunctionCallPart("default", Nil),
    id: String = ByteStr(empty32Bytes).toString,
    fee: Long = FeeConstants(TransactionType.InvokeScript) * FeeUnit,
    feeAssetId: Option[String] = None,
    sender: Option[String] = None,
    senderPublicKey: String = ByteStr(empty32Bytes).toString,
    payment: Seq[Payment] = Nil
) {
  def toInvocation: Either[ValidationError, Invocation] =
    for {
      senderPK <- PublicKey.fromBase58String(senderPublicKey)
      id       <- decodeBase58(id)
      functionCall = InvokeScriptRequest.buildFunctionCall(call)
      feeAssetId <- feeAssetId.traverse(decodeBase58)
      sender <-
        if (sender.nonEmpty || senderPK.arr.sameElements(empty32Bytes))
          sender
            .map(Address.fromString(_, None).map(a => RideAddress(ByteStr(a.bytes))))
            .getOrElse(Right(RideAddress(ByteStr(new Array[Byte](26)))))
        else
          Right(RideAddress(ByteStr(senderPK.toAddress.bytes)))
      payments <- AttachedPaymentExtractor
        .extractPayments(payment, V6, blockchainAllowsMultiPayment = true, smart.DApp)
        .leftMap(GenericError(_))
    } yield Invocation(functionCall, sender, senderPK, sender, senderPK, payments, id, fee, feeAssetId)

  private def decodeBase58(base58: String): Either[ValidationError, ByteStr] =
    ByteStr.decodeBase58(base58).toEither.leftMap(e => GenericError(String.valueOf(e.getMessage)))
}

object UtilsInvocationRequest {
  private val empty32Bytes = new Array[Byte](32)

  implicit val reads: Reads[UtilsInvocationRequest] = Json.using[Json.WithDefaultValues].reads[UtilsInvocationRequest]
}

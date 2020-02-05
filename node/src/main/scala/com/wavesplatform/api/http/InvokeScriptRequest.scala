package com.wavesplatform.api.http

import cats.implicits._
import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import play.api.libs.json._

object InvokeScriptRequest {

  case class FunctionCallPart(function: String, args: List[EVALUATED])

  implicit val EvaluatedReads = new Reads[EVALUATED] {
    def reads(jv: JsValue): JsResult[EVALUATED] = {
      jv \ "type" match {
        case JsDefined(JsString("integer")) =>
          jv \ "value" match {
            case JsDefined(JsNumber(n)) => JsSuccess(CONST_LONG(n.toLong))
            case _                      => JsError("value is missing or not an integer")
          }
        case JsDefined(JsString("boolean")) =>
          jv \ "value" match {
            case JsDefined(JsBoolean(n)) => JsSuccess(CONST_BOOLEAN(n))
            case _                       => JsError("value is missing or not an boolean")
          }
        case JsDefined(JsString("string")) =>
          jv \ "value" match {
            case JsDefined(JsString(n)) => CONST_STRING(n).fold(JsError(_), JsSuccess(_))
            case _                      => JsError("value is missing or not an string")
          }
        case JsDefined(JsString("binary")) =>
          jv \ "value" match {
            case JsDefined(JsString(n)) =>
              ByteStr
                .decodeBase64(n)
                .toEither
                .leftMap(_.getMessage)
                .flatMap(CONST_BYTESTR(_))
                .fold(JsError(_), JsSuccess(_))
            case _ => JsError("value is missing or not an base64 encoded string")
          }
        case _ => JsError("type is missing")
      }
    }
  }

  implicit val functionCallReads                = Json.reads[FunctionCallPart]
  implicit val unsignedInvokeScriptRequestReads = Json.reads[InvokeScriptRequest]
  implicit val signedInvokeScriptRequestReads   = Json.reads[SignedInvokeScriptRequest]

  def buildFunctionCall(fc: FunctionCallPart): FUNCTION_CALL =
    FUNCTION_CALL(FunctionHeader.User(fc.function), fc.args)
}

case class InvokeScriptRequest(
    sender: String,
    fee: Long,
    feeAssetId: Option[String],
    call: Option[InvokeScriptRequest.FunctionCallPart],
    payment: Seq[InvokeScriptTransaction.Payment],
    dApp: String,
    timestamp: Option[Long] = None
)

case class SignedInvokeScriptRequest(
    senderPublicKey: String,
    fee: Long,
    feeAssetId: Option[String],
    dApp: String,
    call: Option[InvokeScriptRequest.FunctionCallPart],
    payment: Option[Seq[InvokeScriptTransaction.Payment]],
    timestamp: Long,
    proofs: List[String]
) extends BroadcastRequest {
  def toTx: Either[ValidationError, InvokeScriptTransaction] =
    for {
      _sender      <- PublicKey.fromBase58String(senderPublicKey)
      _dappAddress <- AddressOrAlias.fromString(dApp)
      _feeAssetId  <- parseBase58ToAssetId(feeAssetId.filter(_.length > 0), "invalid.feeAssetId")
      _proofBytes  <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs      <- Proofs.create(_proofBytes)
      t <- InvokeScriptTransaction.create(
        _sender,
        _dappAddress,
        call.map(fCallPart => InvokeScriptRequest.buildFunctionCall(fCallPart)),
        payment.getOrElse(Seq()),
        fee,
        _feeAssetId,
        timestamp,
        _proofs
      )
    } yield t
}

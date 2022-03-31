package com.wavesplatform.api.http.requests

import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
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

  implicit val EvaluatedReads: Reads[EVALUATED] = (jv: JsValue) => {
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
      case JsDefined(JsString("list")) =>
        ARR((jv \ "value").as[Vector[EVALUATED]], true).fold(JsError.apply, ({v: EVALUATED => JsSuccess(v)}))
      case _ => JsError("type is missing")
    }
  }

  implicit val functionCallReads: Reads[FunctionCallPart] = (jv: JsValue) => {
    val argsR = jv \ "args" match {
      case JsDefined(JsArray(value)) => value.toList.traverse(EvaluatedReads.reads)
      case JsDefined(_)              => JsError("Unexpected args format")
      case _: JsUndefined            => JsSuccess(Nil)
    }
    val funcNameR = jv \ "function" match {
      case JsDefined(JsString(value)) => JsSuccess(value)
      case JsDefined(_)               => JsError("Unexpected call function name format")
      case _: JsUndefined             => JsError("Undefined call function name")
    }
    for {
      args     <- argsR
      funcName <- funcNameR
    } yield FunctionCallPart(funcName, args)
  }

  implicit val unsignedInvokeScriptRequestReads: Reads[InvokeScriptRequest]     = Json.reads[InvokeScriptRequest]
  implicit val signedInvokeScriptRequestReads: Reads[SignedInvokeScriptRequest] = Json.reads[SignedInvokeScriptRequest]

  def buildFunctionCall(fc: FunctionCallPart): FUNCTION_CALL =
    FUNCTION_CALL(FunctionHeader.User(fc.function), fc.args)
}

case class InvokeScriptRequest(
    version: Option[Byte],
    sender: String,
    fee: Long,
    feeAssetId: Option[String],
    call: Option[InvokeScriptRequest.FunctionCallPart],
    payment: Seq[InvokeScriptTransaction.Payment],
    dApp: String,
    timestamp: Option[Long] = None
)

case class SignedInvokeScriptRequest(
    version: Option[Byte],
    senderPublicKey: String,
    fee: Long,
    feeAssetId: Option[String],
    dApp: String,
    call: Option[InvokeScriptRequest.FunctionCallPart],
    payment: Option[Seq[InvokeScriptTransaction.Payment]],
    timestamp: Long,
    proofs: Proofs
) {
  def toTx: Either[ValidationError, InvokeScriptTransaction] =
    for {
      _sender      <- PublicKey.fromBase58String(senderPublicKey)
      _dappAddress <- AddressOrAlias.fromString(dApp, checkChainId = false)
      _feeAssetId  <- parseBase58ToAsset(feeAssetId.filter(_.length > 0), "invalid.feeAssetId")
      t <- InvokeScriptTransaction.create(
        version.getOrElse(2.toByte),
        _sender,
        _dappAddress,
        call.map(fCallPart => InvokeScriptRequest.buildFunctionCall(fCallPart)),
        payment.getOrElse(Seq()),
        fee,
        _feeAssetId,
        timestamp,
        proofs
      )
    } yield t
}

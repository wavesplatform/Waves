package com.wavesplatform.api.http.requests

import cats.instances.list.*
import cats.syntax.either.*
import cats.syntax.traverse.*
import com.wavesplatform.account.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.transaction.{Asset, Proofs}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, InvokeTransaction}
import play.api.libs.json.*

object InvokeScriptRequest {

  case class FunctionCallPart(function: String, args: List[EVALUATED] = Nil)

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
          case JsDefined(JsString(n)) => CONST_STRING(n).fold(e => JsError(e.message), JsSuccess(_))
          case _                      => JsError("value is missing or not an string")
        }
      case JsDefined(JsString("binary")) =>
        jv \ "value" match {
          case JsDefined(JsString(n)) =>
            ByteStr
              .decodeBase64(n)
              .toEither
              .leftMap(_.getMessage)
              .flatMap(r => CONST_BYTESTR(r).leftMap(_.message))
              .fold(JsError(_), JsSuccess(_))
          case _ => JsError("value is missing or not an base64 encoded string")
        }
      case JsDefined(JsString("list")) =>
        ARR((jv \ "value").as[Vector[EVALUATED]], true).fold(
          e => JsError(e.message),
          { (v: EVALUATED) => JsSuccess(v) }
        )
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
    chainId: Option[Byte],
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
    chainId: Option[Byte],
    version: Option[Byte],
    senderPublicKey: String,
    fee: Long,
    feeAssetId: Option[Asset],
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
      t <- InvokeScriptTransaction.create(
        version.getOrElse(2.toByte),
        _sender,
        _dappAddress,
        call.map(InvokeScriptRequest.buildFunctionCall).filterNot(_ == InvokeTransaction.DefaultCall),
        payment.getOrElse(Seq()),
        fee,
        feeAssetId.getOrElse(Asset.Waves),
        timestamp,
        proofs,
        chainId.getOrElse(_dappAddress.chainId)
      )
    } yield t
}

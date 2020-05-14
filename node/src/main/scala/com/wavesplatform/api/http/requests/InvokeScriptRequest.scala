package com.wavesplatform.api.http.requests

import cats.implicits._
import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.MaxListLengthV4
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import play.api.libs.json._

object InvokeScriptRequest {

  case class FunctionCallPart(function: String, args: List[EVALUATED])

  implicit val EvaluatedReads: Reads[EVALUATED] = (jv: JsValue) => {
    def read(jv: JsValue): JsResult[EVALUATED] =
      jv \ "type" match {
        case JsDefined(JsString("integer")) =>
          jv \ "value" match {
            case JsDefined(JsNumber(n)) => JsSuccess(CONST_LONG(n.toLong))
            case _ => JsError("value is missing or not an integer")
          }
        case JsDefined(JsString("boolean")) =>
          jv \ "value" match {
            case JsDefined(JsBoolean(n)) => JsSuccess(CONST_BOOLEAN(n))
            case _ => JsError("value is missing or not a boolean")
          }
        case JsDefined(JsString("string")) =>
          jv \ "value" match {
            case JsDefined(JsString(n)) => CONST_STRING(n).fold(JsError(_), JsSuccess(_))
            case _ => JsError("value is missing or not a string")
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
            case _ => JsError("value is missing or not a base64 encoded string")
          }
        case JsDefined(JsString("list")) =>
          jv \ "value" match {
            case JsDefined(JsArray(args)) =>
              for {
                parsedArgs <- args.toStream.traverse(read)
                arr        <- ARR(parsedArgs.toIndexedSeq, limited = false).fold(JsError(_), JsSuccess(_))
              } yield arr
            case _ => JsError("value is missing or not a list")
          }
        case _ => JsError("type is missing")
      }

    read(jv)
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

  def buildFunctionCall(fc: FunctionCallPart): Either[ValidationError, FUNCTION_CALL] =
    Either.cond(
      fc.args.collect { case arr: ARR if arr.xs.size > MaxListLengthV4 => arr }.isEmpty,
      FUNCTION_CALL(FunctionHeader.User(fc.function), fc.args),
      GenericError(s"List size should not exceed $MaxListLengthV4")
    )
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
      _dappAddress <- AddressOrAlias.fromString(dApp)
      _feeAssetId  <- parseBase58ToAsset(feeAssetId.filter(_.length > 0), "invalid.feeAssetId")
      checkedCall  <- call.traverse(InvokeScriptRequest.buildFunctionCall)
      t <- InvokeScriptTransaction.create(
        version.getOrElse(2.toByte),
        _sender,
        _dappAddress,
        checkedCall,
        payment.getOrElse(Seq()),
        fee,
        _feeAssetId,
        timestamp,
        proofs
      )
    } yield t
}

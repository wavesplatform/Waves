package com.wavesplatform.api.http

import cats.implicits._
import com.wavesplatform.account.{Address, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.{Proofs, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json._

import scala.annotation.meta.field

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
            case JsDefined(JsString(n)) => JsSuccess(CONST_STRING(n))
            case _                      => JsError("value is missing or not an string")
          }
        case JsDefined(JsString("binary")) =>
          jv \ "value" match {
            case JsDefined(JsString(n)) =>
              ByteStr.decodeBase64(n).fold(ex => JsError(ex.getMessage), bstr => JsSuccess(CONST_BYTESTR(bstr)))
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

case class InvokeScriptRequest(sender: String,
                               @(ApiModelProperty @field)(required = true, value = "1000") fee: Long,
                               @(ApiModelProperty @field)(
                                 dataType = "string",
                                 example = "3Z7T9SwMbcBuZgcn3mGu7MMp619CTgSWBT7wvEkPwYXGnoYzLeTyh3EqZu1ibUhbUHAsGK5tdv9vJL9pk4fzv9Gc",
                                 required = false
                               ) feeAssetId: Option[String],
                               @(ApiModelProperty @field)(required = true) call: InvokeScriptRequest.FunctionCallPart,
                               @(ApiModelProperty @field)(required = true) payment: Seq[InvokeScriptTransaction.Payment],
                               @(ApiModelProperty @field)(dataType = "string", example = "3Mciuup51AxRrpSz7XhutnQYTkNT9691HAk") dappAddress: String,
                               timestamp: Option[Long] = None)

@ApiModel(value = "Signed Invoke script transaction")
case class SignedInvokeScriptRequest(
    @(ApiModelProperty @field)(value = "Base58 encoded sender public key", required = true) senderPublicKey: String,
    @(ApiModelProperty @field)(required = true) fee: Long,
    @(ApiModelProperty @field)(
      dataType = "string",
      example = "3Z7T9SwMbcBuZgcn3mGu7MMp619CTgSWBT7wvEkPwYXGnoYzLeTyh3EqZu1ibUhbUHAsGK5tdv9vJL9pk4fzv9Gc",
      required = false
    ) feeAssetId: Option[String],
    @(ApiModelProperty @field)(dataType = "string", example = "3Mciuup51AxRrpSz7XhutnQYTkNT9691HAk") dappAddress: String,
    @(ApiModelProperty @field)(required = true) call: InvokeScriptRequest.FunctionCallPart,
    @(ApiModelProperty @field)(required = true) payment: Option[Seq[InvokeScriptTransaction.Payment]],
    @(ApiModelProperty @field)(required = true, value = "1000") timestamp: Long,
    @(ApiModelProperty @field)(required = true) proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, InvokeScriptTransaction] =
    for {
      _sender      <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _dappAddress <- Address.fromString(dappAddress)
      _feeAssetId  <- parseBase58ToAssetId(feeAssetId.filter(_.length > 0), "invalid.feeAssetId")
      _proofBytes  <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs      <- Proofs.create(_proofBytes)
      t <- InvokeScriptTransaction.create(
        _sender,
        _dappAddress,
        InvokeScriptRequest.buildFunctionCall(call),
        payment.getOrElse(Seq()),
        fee,
        _feeAssetId,
        timestamp,
        _proofs
      )
    } yield t
}

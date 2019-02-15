package com.wavesplatform.api.http

import cats.implicits._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.account.{Address, PublicKeyAccount}
import com.wavesplatform.transaction.smart.ContractInvocationTransaction
import com.wavesplatform.transaction.{Proofs, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json._

import scala.annotation.meta.field

object ContractInvocationRequest {

  case class FunctionCallPart(function: String, args: List[EVALUATED])

  implicit val EVALUATED_Reads = new Reads[EVALUATED] {
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

  implicit val functionCallReads                      = Json.reads[FunctionCallPart]
  implicit val unsignedContractInvocationRequestReads = Json.reads[ContractInvocationRequest]
  implicit val signedContractInvocationRequestReads   = Json.reads[SignedContractInvocationRequest]

  def buildFunctionCall(fc: FunctionCallPart): FUNCTION_CALL =
    FUNCTION_CALL(FunctionHeader.User(fc.function), fc.args)
}

case class ContractInvocationRequest(
    sender: String,
    @(ApiModelProperty @field)(required = true, value = "1000") fee: Long,
    @(ApiModelProperty @field)(required = true) call: ContractInvocationRequest.FunctionCallPart,
    @(ApiModelProperty @field)(required = true) payment: Option[ContractInvocationTransaction.Payment],
    @(ApiModelProperty @field)(dataType = "string", example = "3Mciuup51AxRrpSz7XhutnQYTkNT9691HAk") contractAddress: String,
    timestamp: Option[Long] = None)
@ApiModel(value = "Signed Data transaction")
case class SignedContractInvocationRequest(
    @(ApiModelProperty @field)(value = "Base58 encoded sender public key", required = true) senderPublicKey: String,
    @(ApiModelProperty @field)(required = true) fee: Long,
    @(ApiModelProperty @field)(dataType = "string", example = "3Mciuup51AxRrpSz7XhutnQYTkNT9691HAk") contractAddress: String,
    @(ApiModelProperty @field)(required = true) call: ContractInvocationRequest.FunctionCallPart,
    @(ApiModelProperty @field)(required = true) payment: Option[ContractInvocationTransaction.Payment],
    @(ApiModelProperty @field)(required = true, value = "1000") timestamp: Long,
    @(ApiModelProperty @field)(required = true) proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, ContractInvocationTransaction] =
    for {
      _sender          <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _contractAddress <- Address.fromString(contractAddress)
      _proofBytes      <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs          <- Proofs.create(_proofBytes)
      t <- ContractInvocationTransaction.create(
        _sender,
        _contractAddress,
        ContractInvocationRequest.buildFunctionCall(call),
        payment,
        fee,
        timestamp,
        _proofs
      )
    } yield t
}

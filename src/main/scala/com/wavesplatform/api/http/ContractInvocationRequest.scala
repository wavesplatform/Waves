package com.wavesplatform.api.http

import cats.implicits._
import com.wavesplatform.account.{Address, PublicKeyAccount}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, DataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.smart.ContractInvocationTransaction
import com.wavesplatform.transaction.{Proofs, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.Json

import scala.annotation.meta.field

object ContractInvocationRequest {

  case class FunctionCallPart(function: String, args: List[DataEntry[_]])
  implicit val functionCallReads = Json.reads[FunctionCallPart]

  implicit val unsignedContractInvocationRequestReads = Json.reads[ContractInvocationRequest]
  implicit val signedContractInvocationRequestReads   = Json.reads[SignedContractInvocationRequest]

  def buildFunctionCall(fc: FunctionCallPart): FUNCTION_CALL =
    FUNCTION_CALL(
      FunctionHeader.User(fc.function),
      fc.args.map {
        case BooleanDataEntry(_, b) => CONST_BOOLEAN(b)
        case StringDataEntry(_, b)  => CONST_STRING(b)
        case IntegerDataEntry(_, b) => CONST_LONG(b)
        case BinaryDataEntry(_, b)  => CONST_BYTESTR(b)
      }
    )
}

case class ContractInvocationRequest(
    @(ApiModelProperty @field)(required = true, dataType = "java.lang.Integer", value = "1", allowableValues = "1") version: Byte,
    sender: String,
    @(ApiModelProperty @field)(required = true, value = "1000") fee: Long,
    @(ApiModelProperty @field)(required = true) call: ContractInvocationRequest.FunctionCallPart,
    @(ApiModelProperty @field)(required = true) payment: Option[ContractInvocationTransaction.Payment],
    @(ApiModelProperty @field)(dataType = "string", example = "3Mciuup51AxRrpSz7XhutnQYTkNT9691HAk") contractAddress: String,
    timestamp: Option[Long] = None)
@ApiModel(value = "Signed Data transaction")
case class SignedContractInvocationRequest(
    @(ApiModelProperty @field)(required = true, dataType = "java.lang.Integer", value = "1", allowableValues = "1")
    version: Byte,
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
        version,
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

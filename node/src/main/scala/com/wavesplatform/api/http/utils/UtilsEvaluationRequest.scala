package com.wavesplatform.api.http.utils

import cats.implicits.toTraverseOps
import cats.syntax.either.*
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.http.requests.InvokeScriptRequest
import com.wavesplatform.api.http.requests.InvokeScriptRequest.FunctionCallPart
import com.wavesplatform.api.http.utils.UtilsApiRoute.DefaultPublicKey
import com.wavesplatform.api.http.utils.UtilsEvaluator.{ConflictingRequestStructure, ParseJsonError}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.values.{Expression, StdLibVersion, V6}
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.compiler.{ExpressionCompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.Invocation
import com.wavesplatform.lang.v1.parser.Parser.LibrariesOffset.NoLibraries
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address as RideAddress
import com.wavesplatform.lang.{ValidationError, utils}
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.state.{Blockchain, BlockchainOverrides, OverriddenBlockchain}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.AttachedPaymentExtractor
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.{TransactionType, smart}
import play.api.libs.json.*

sealed trait UtilsEvaluationRequest {
  def state: Option[BlockchainOverrides]
  def mkBlockchain(underlying: Blockchain): Blockchain = state.foldLeft(underlying)(new OverriddenBlockchain(_, _))
}

object UtilsEvaluationRequest {
  // This should be Reads, but we will broke REST API
  def parse(request: JsObject): Either[ValidationError, UtilsEvaluationRequest] = {
    def withoutCommonFields = request - "state"
    (request.validate[UtilsExprRequest], request.validate[UtilsInvocationRequest]) match {
      case (JsSuccess(_, _), JsSuccess(_, _)) if withoutCommonFields.fields.size > 1 =>
        ConflictingRequestStructure.asLeft
      case (JsSuccess(exprRequest, _), _)       => exprRequest.asRight
      case (_, JsSuccess(invocationRequest, _)) => invocationRequest.asRight
      case (_, e: JsError)                      => ParseJsonError(e).asLeft
    }
  }
}

case class UtilsExprRequest(
    expr: String,
    state: Option[BlockchainOverrides] = None
) extends UtilsEvaluationRequest {
  def parseCall(version: StdLibVersion): Either[GenericError, Terms.EXPR] = compile(version, expr)

  private def compile(version: StdLibVersion, str: String): Either[GenericError, EXPR] =
    ExpressionCompiler
      .compileUntyped(str, NoLibraries, utils.compilerContext(version, Expression, isAssetScript = false).copy(arbitraryDeclarations = true))
      .leftMap(GenericError(_))
}

object UtilsExprRequest {
  implicit val utilsExprRequestReads: Reads[UtilsExprRequest] = Json.using[Json.WithDefaultValues].reads[UtilsExprRequest]
}

case class UtilsInvocationRequest(
    call: FunctionCallPart = FunctionCallPart("default", Nil),
    id: String = ByteStr(empty32Bytes).toString,
    fee: Long = FeeConstants(TransactionType.InvokeScript) * FeeUnit,
    feeAssetId: Option[String] = None,
    sender: Option[String] = None,
    senderPublicKey: String = DefaultPublicKey.toString,
    payment: Seq[Payment] = Nil,
    state: Option[BlockchainOverrides] = None
) extends UtilsEvaluationRequest {
  def toInvocation: Either[ValidationError, Invocation] =
    for {
      senderPK <- PublicKey.fromBase58String(senderPublicKey)
      id       <- decodeBase58(id)
      functionCall = InvokeScriptRequest.buildFunctionCall(call)
      feeAssetId <- feeAssetId.traverse(decodeBase58)
      senderAddress <- sender
        .fold(RideAddress(ByteStr(senderPK.toAddress.bytes)).asRight[ValidationError]) { s =>
          Address.fromString(s).map(a => RideAddress(ByteStr(a.bytes)))
        }
      payments <- AttachedPaymentExtractor
        .extractPayments(payment, V6, blockchainAllowsMultiPayment = true, smart.DApp)
        .leftMap(GenericError(_))
    } yield Invocation(functionCall, senderAddress, senderPK, senderAddress, senderPK, payments, id, fee, feeAssetId)

  private def decodeBase58(base58: String): Either[ValidationError, ByteStr] =
    ByteStr.decodeBase58(base58).toEither.leftMap(e => GenericError(String.valueOf(e.getMessage)))
}

object UtilsInvocationRequest {
  implicit val reads: Reads[UtilsInvocationRequest] = Json.using[Json.WithDefaultValues].reads[UtilsInvocationRequest]
}

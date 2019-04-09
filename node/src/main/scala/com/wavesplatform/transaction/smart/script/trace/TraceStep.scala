package com.wavesplatform.transaction.smart.script.trace

import com.wavesplatform.account.Address
import com.wavesplatform.api.http.ApiError
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.evaluator.ScriptResult
import com.wavesplatform.transaction.ValidationError
import play.api.libs.json.{JsObject, Json}

sealed abstract class TraceStep {
  def json: JsObject
}

case class AccountVerifierTrace(
    address: Address,
    errorO:  Option[ValidationError]
) extends TraceStep {

  override def json: JsObject = Json.obj(
    "address" -> address.address,
  ) ++ (errorO match {
    case Some(e) => Json.obj("error"  -> ApiError.fromValidationError(e).message)
    case None    => Json.obj("result" -> "ok")
  })
}

case class AssetVerifierTrace(
    id:     ByteStr,
    errorO: Option[ValidationError]
) extends TraceStep {
  override def json: JsObject = Json.obj(
    "address" -> id.base58,
  ) ++ (errorO match {
    case Some(e) => Json.obj("error"  -> ApiError.fromValidationError(e).message)
    case None    => Json.obj("result" -> "ok")
  })
}

case class InvokeScriptTrace(
    dAppAddress: Address,
    function:    FUNCTION_CALL,
    resultE:     Either[ValidationError, ScriptResult]
) extends TraceStep {
  override def json: JsObject =
    Json.obj(
      "dAppAddress" -> dAppAddress.address,
      "function"    -> function.function.funcName,
      "args"        -> function.args.map(_.toString),
      resultE match {
        case Right(value) => "result" -> value.toString
        case Left(e)      => "error"  -> ApiError.fromValidationError(e).message
      }
    )
}


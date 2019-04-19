package com.wavesplatform.transaction.smart.script.trace

import com.wavesplatform.account.Address
import com.wavesplatform.api.http.ApiError
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.evaluator.ScriptResult
import play.api.libs.json.{JsObject, Json}

sealed abstract class TraceStep {
  def json: JsObject
}

case class AccountVerifierTrace(
    address: Address,
    errorO:  Option[ValidationError]
) extends TraceStep {

  override lazy val json: JsObject = Json.obj(
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
  override lazy val json: JsObject = Json.obj(
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

  override lazy val json: JsObject =
    Json.obj(
      "dAppAddress" -> dAppAddress.address,
      "function"    -> function.function.funcName,
      "args"        -> function.args.map(_.toString),
      resultE match {
        case Right(value) => "result" -> toJson(value)
        case Left(e)      => "error"  -> ApiError.fromValidationError(e).message
      }
    )

  private def toJson(v: ScriptResult) = Json.obj(
    "data" -> v.ds.map(item => Json.obj(
      "key"   -> item.key,
      "value" -> item.value.toString
    )),
    "transfers" -> v.ts.map { case (address, amount, assetId) => Json.obj(
      "address" -> address.bytes.toString,
      "amount"  -> amount,
      "asset"   -> (assetId match {
        case Some(id) => id.toString
        case None     => "Waves"
      })
    )}
  )
}


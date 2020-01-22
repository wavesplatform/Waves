package com.wavesplatform.transaction.smart.script.trace

import cats.Id
import com.wavesplatform.account.{Address, AddressOrAlias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.evaluator.{Log, ScriptResult}
import com.wavesplatform.transaction.TxValidationError.{ScriptExecutionError, TransactionNotAllowedByScript}
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json._

sealed abstract class TraceStep {
  def json: JsObject
  def loggedJson = json
}

case class AccountVerifierTrace(
    address: Address,
    errorO: Option[ValidationError]
) extends TraceStep {

  override lazy val json: JsObject = Json.obj(
    "address" -> address.stringRepr,
  ) ++ (errorO match {
    case Some(e) => Json.obj("error"  -> TraceStep.errorJson(e))
    case None    => Json.obj("result" -> "ok")
  })
}

case class AssetVerifierTrace(
    id: ByteStr,
    errorO: Option[ValidationError]
) extends TraceStep {
  override lazy val json: JsObject = Json.obj(
    "assetId" -> id.base58,
  ) ++ (errorO match {
    case Some(e) => Json.obj("error"  -> TraceStep.errorJson(e))
    case None    => Json.obj("result" -> "ok")
  })
}

case class InvokeScriptTrace(
    dAppAddressOrAlias: AddressOrAlias,
    functionCall: FUNCTION_CALL,
    resultE: Either[ValidationError, ScriptResult],
    log: Log[Id]
) extends TraceStep {

  override lazy val json: JsObject = maybeLogged(false)

  override lazy val loggedJson: JsObject = maybeLogged(true)

  private def maybeLogged(logged: Boolean): JsObject = {
    Json.obj(
      "dApp"     -> dAppAddressOrAlias.stringRepr,
      "function" -> functionCall.function.funcName,
      "args"     -> functionCall.args.map(_.toString),
      resultE match {
        case Right(value) => "result" ->
           ({v: JsObject => if(logged) {
              v ++ Json.obj(TraceStep.logJson(log))
           } else {
              v
           }})(toJson(value))
        case Left(e)      => "error"  -> TraceStep.errorJson(e)
      }
    )
  }

  private def toJson(v: ScriptResult) = Json.obj(
    "data" -> v.ds.map(
      item =>
        Json.obj(
          "key"   -> item.key,
          "value" -> item.value.toString
      )),
    "transfers" -> v.ts.map {
      case (address, amount, assetId) =>
        Json.obj(
          "address" -> address.bytes.toString,
          "amount"  -> amount,
          "assetId" -> (assetId match {
            case Some(id) => id.toString
            case None     => JsNull
          })
        )
    }
  )
}

object TraceStep {
  def errorJson(e: ValidationError): JsValue = e match {
    case ScriptExecutionError(error, log, isAssetScript)   => Json.obj(logType(isAssetScript), logJson(log), "reason" -> error)
    case TransactionNotAllowedByScript(log, isAssetScript) => Json.obj(logType(isAssetScript), logJson(log))
    case a                                                 => JsString(a.toString)
  }

  private def logType(isAssetScript: Boolean): (String, JsValueWrapper) =
    "type" -> (if (isAssetScript) "Asset" else "Account")

  def logJson(l: Log[Id]): (String, JsValueWrapper) =
    "vars" -> l.map {
      case (k, Right(v))  => Json.obj("name" -> k, "value" -> v.toString)
      case (k, Left(err)) => Json.obj("name" -> k, "error" -> err)
    }
}

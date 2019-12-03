package com.wavesplatform.transaction.smart.script.trace

import cats.Id
import com.wavesplatform.account.{Address, AddressOrAlias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.evaluator.{Log, ScriptResult, ScriptResultV3, ScriptResultV4}
import com.wavesplatform.lang.v1.traits.domain.{AssetTransfer, Burn, DataItem, Issue, Reissue}
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
    "assetId" -> id.toString,
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

  private def toJson(v: ScriptResult) =
    v match {
      case ScriptResultV3(ds, ts) =>
        Json.obj(
          "data"      -> ds.map(dataItemJson),
          "transfers" -> ts.map(transferJson)
        )
      case ScriptResultV4(actions) =>
        Json.obj(
          "actions" -> actions.map {
            case transfer: AssetTransfer => transferJson(transfer) + ("type" -> JsString("transfer"))
            case issue: Issue            => issueJson(issue)       + ("type" -> JsString("issue"))
            case reissue: Reissue        => reissueJson(reissue)   + ("type" -> JsString("reissue"))
            case burn: Burn              => burnJson(burn)         + ("type" -> JsString("burn"))
            case item: DataItem[_]       => dataItemJson(item)     + ("type" -> JsString("dataItem"))
          }
        )
    }

  private def transferJson(transfer: AssetTransfer) =
    Json.obj(
      "address" -> transfer.recipient.bytes.toString,
      "amount"  -> transfer.amount,
      "assetId" -> (transfer.assetId match {
        case Some(id) => id.toString
        case None     => JsNull
      })
    )

  private def dataItemJson(item: DataItem[_]) =
    Json.obj(
      "key"   -> item.key,
      "value" -> item.value.toString
    )

  private def issueJson(issue: Issue) =
    Json.obj(
      "script" -> (issue.compiledScript match {
        case Some(script) => script.toString
        case None         => JsNull
      }),
      "decimals"    -> issue.decimals,
      "description" -> issue.description,
      "reissuable"  -> issue.isReissuable,
      "quantity"    -> issue.quantity,
      "name"        -> issue.name
    )

  private def reissueJson(reissue: Reissue) =
    Json.obj(
      "assetId"      -> reissue.assetId.toString,
      "isReissuable" -> reissue.isReissuable,
      "quantity"     -> reissue.quantity
    )

  private def burnJson(burn: Burn) =
    Json.obj(
      "assetId"  -> burn.assetId.toString,
      "quantity" -> burn.quantity
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

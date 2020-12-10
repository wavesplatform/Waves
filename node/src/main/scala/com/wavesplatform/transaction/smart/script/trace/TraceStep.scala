package com.wavesplatform.transaction.smart.script.trace

import cats.Id
import com.wavesplatform.account.{Address, AddressOrAlias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.evaluator.{Log, ScriptResult}
import com.wavesplatform.state.InvokeScriptResult
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError.{ScriptExecutionError, TransactionNotAllowedByScript}
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json._

sealed abstract class TraceStep {
  def json: JsObject
  def loggedJson: JsObject = json
}

case class AccountVerifierTrace(
    address: Address,
    errorO: Option[ValidationError]
) extends TraceStep {

  override lazy val json: JsObject = Json.obj(
    "type" -> "verifier",
    "id"   -> address.stringRepr
  ) ++ (errorO match {
    case Some(e) => Json.obj("error"  -> TraceStep.errorJson(e))
    case None    => Json.obj("result" -> "ok")
  })
}

object AssetVerifierTrace {
  type AssetContext = AssetContext.Value
  object AssetContext extends Enumeration {
    val Unknown, OrderAmount, OrderPrice, MatcherFee, Payment, Reissue, Burn, Sponsor, Transfer, UpdateInfo = Value

    def fromTxAndAsset(tx: Transaction, asset: IssuedAsset): AssetContext = tx match {
      case i: InvokeScriptTransaction if i.payments.exists(_.assetId == asset) => AssetContext.Payment

      case e: ExchangeTransaction if e.order1.assetPair.amountAsset == asset                            => AssetContext.OrderAmount
      case e: ExchangeTransaction if e.order1.assetPair.priceAsset == asset                             => AssetContext.OrderPrice
      case e: ExchangeTransaction if Set(e.order1.matcherFeeAssetId, e.order2.matcherFeeAssetId)(asset) => AssetContext.OrderPrice

      case r: ReissueTransaction if r.asset == asset           => AssetContext.Reissue
      case r: BurnTransaction if r.asset == asset              => AssetContext.Burn
      case s: SponsorFeeTransaction if s.asset == asset        => AssetContext.Sponsor
      case u: UpdateAssetInfoTransaction if u.assetId == asset => AssetContext.UpdateInfo
      case u: SetAssetScriptTransaction if u.asset == asset    => AssetContext.UpdateInfo

      case t: TransferTransaction if t.assetId == asset       => AssetContext.Transfer
      case mt: MassTransferTransaction if mt.assetId == asset => AssetContext.Transfer

      case _ => AssetContext.Unknown
    }
  }
}

case class AssetVerifierTrace(
    id: ByteStr,
    errorOpt: Option[ValidationError],
    context: AssetVerifierTrace.AssetContext = AssetVerifierTrace.AssetContext.Unknown
) extends TraceStep {
  override lazy val json: JsObject = Json.obj(
    "type"    -> "asset",
    "context" -> context.toString.updated(0, context.toString.charAt(0).toLower),
    "id"      -> id.toString
  ) ++ (errorOpt match {
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
  override lazy val json: JsObject       = maybeLoggedJson(false)
  override lazy val loggedJson: JsObject = maybeLoggedJson(true)

  private[this] def maybeLoggedJson(logged: Boolean): JsObject = {
    Json.obj(
      "type"     -> "dApp",
      "id"       -> dAppAddressOrAlias.stringRepr,
      "function" -> functionCall.function.funcName,
      "args"     -> functionCall.args.map(_.toString),
      resultE match {
        case Right(value) =>
          "result" -> (TraceStep.scriptResultJson(value) ++ (if (logged) Json.obj(TraceStep.logJson(log)) else JsObject.empty))
        case Left(e) => "error" -> TraceStep.errorJson(e)
      }
    )
  }

}

object TraceStep {
  def scriptResultJson(v: ScriptResult): JsObject =
    Json.toJsObject(InvokeScriptResult.fromLangResult(v))

  def errorJson(e: ValidationError): JsValue = e match {
    case see: ScriptExecutionError          => Json.obj(logType(see.isAssetScript), logJson(see.log), "reason" -> see.error)
    case tne: TransactionNotAllowedByScript => Json.obj(logType(tne.isAssetScript), logJson(tne.log))
    case a                                  => JsString(a.toString)
  }

  private def logType(isAssetScript: Boolean): (String, JsValueWrapper) =
    "type" -> (if (isAssetScript) "asset" else "account")

  def logJson(l: Log[Id]): (String, JsValueWrapper) =
    "vars" -> l.map {
      case (k, Right(v))  => Json.obj("name" -> k) ++ serializeVarValue(v)
      case (k, Left(err)) => Json.obj("name" -> k, "error" -> err)
    }

  import com.wavesplatform.lang.v1.compiler.Terms
  def serializeVarValue(e: Terms.EVALUATED): JsObject = e match { // TODO: Merge with https://github.com/wavesplatform/Waves/pull/3305/files
    case Terms.CONST_LONG(t)     => Json.obj("type" -> "Int", "value"        -> t)
    case bs: Terms.CONST_BYTESTR => Json.obj("type" -> "ByteVector", "value" -> bs.bs.toString)
    case s: Terms.CONST_STRING   => Json.obj("type" -> "String", "value"     -> s.s)
    case Terms.CONST_BOOLEAN(b)  => Json.obj("type" -> "Boolean", "value"    -> b)
    case Terms.CaseObj(caseType, fields) =>
      Json.obj("type" -> caseType.name, "value" -> JsObject(fields.view.mapValues(serializeVarValue).toSeq))
    case Terms.ARR(xs)      => Json.obj("type" -> "Array", "value" -> xs.map(serializeVarValue))
    case Terms.FAIL(reason) => Json.obj("type" -> "Fail", "value"  -> reason)
  }
}

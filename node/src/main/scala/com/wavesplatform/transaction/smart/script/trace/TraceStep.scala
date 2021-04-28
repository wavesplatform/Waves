package com.wavesplatform.transaction.smart.script.trace

import cats.Id
import com.wavesplatform.account.{Address, AddressOrAlias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.evaluator.{Log, ScriptResult}
import com.wavesplatform.serialization.ScriptValuesJson
import com.wavesplatform.state.InvokeScriptResult
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError.{FailedTransactionError, ScriptExecutionError, TransactionNotAllowedByScript}
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import play.api.libs.json._
import play.api.libs.json.Json.JsValueWrapper

sealed abstract class TraceStep {
  def json: JsObject // TODO: Is this format necessary?
  def loggedJson: JsObject = json
}

case class AccountVerifierTrace(
    address: Address,
    errorOpt: Option[ValidationError]
) extends TraceStep {

  override lazy val json: JsObject = Json
    .obj(
      "type" -> "verifier",
      "id"   -> address.stringRepr
    ) ++ TraceStep.maybeErrorJson(errorOpt)
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
  ) ++ TraceStep.maybeErrorJson(errorOpt)
}

case class InvokeScriptTrace(
    invokeId: ByteStr,
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
      "args"     -> functionCall.args.map(_.toString)
    ) ++ (resultE match {
      case Right(value) => TraceStep.maybeErrorJson(None) ++ Json.obj("result" -> TraceStep.scriptResultJson(invokeId, value))
      case Left(e)      => TraceStep.maybeErrorJson(Some(e))
    }) ++ (if (logged) Json.obj(TraceStep.logJson(log)) else JsObject.empty)
  }
}

object TraceStep {
  private[trace] def scriptResultJson(invokeId: ByteStr, v: ScriptResult): JsObject =
    Json.toJsObject(InvokeScriptResult.fromLangResult(invokeId, v))

  private[trace] def maybeErrorJson(errorOpt: Option[ValidationError]): JsObject =
    errorOpt match {
      case Some(e) => Json.obj("result" -> "failure") ++ TraceStep.errorJson(e)
      case None    => Json.obj("result" -> "success", "error" -> JsNull)
    }

  private def errorJson(e: ValidationError): JsObject = e match {
    case see: ScriptExecutionError          => Json.obj(logJson(see.log), "error" -> see.error)
    case tne: TransactionNotAllowedByScript => Json.obj(logJson(tne.log), "error" -> JsNull)
    case fte: FailedTransactionError        => Json.obj(logJson(fte.log), "error" -> fte.error.map(JsString))
    case a                                  => Json.obj("error"                   -> a.toString)
  }

  private[trace] def logJson(l: Log[Id]): (String, JsValueWrapper) =
    "vars" -> l.map {
      case (k, Right(v))  => Json.obj("name" -> k) ++ ScriptValuesJson.serializeValue(v)
      case (k, Left(err)) => Json.obj("name" -> k, "error" -> err)
    }
}

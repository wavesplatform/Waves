package com.wavesplatform.api.http.utils

import cats.Id
import cats.implicits.catsSyntaxSemigroup
import cats.syntax.either.*
import com.wavesplatform.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.api.http.ApiError
import com.wavesplatform.api.http.ApiError.ScriptExecutionError
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.EstimatorProvider.*
import com.wavesplatform.features.EvaluatorFixProvider.*
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{DApp as DAppType, *}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, EXPR}
import com.wavesplatform.lang.v1.compiler.{ContractScriptCompactor, ExpressionCompiler}
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.LogExtraInfo
import com.wavesplatform.lang.v1.evaluator.{EvaluatorV2, Log, ScriptResult}
import com.wavesplatform.lang.v1.parser.Parser.LibrariesOffset.NoLibraries
import com.wavesplatform.lang.v1.traits.Environment.Tthis
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.lang.{ValidationError, utils}
import com.wavesplatform.serialization.ScriptValuesJson
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.state.diffs.invoke.{InvokeDiffsCommon, InvokeScriptTransactionLike, StructuredCallableActions}
import com.wavesplatform.state.reader.SnapshotBlockchain
import com.wavesplatform.state.{AccountScriptInfo, Blockchain, InvokeScriptResult, Portfolio, StateSnapshot}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TransactionType.InvokeScript
import com.wavesplatform.transaction.TxValidationError.{GenericError, InvokeRejectError}
import com.wavesplatform.transaction.smart.*
import com.wavesplatform.transaction.smart.DAppEnvironment.ActionLimits
import com.wavesplatform.transaction.smart.script.trace.TraceStep
import com.wavesplatform.transaction.validation.impl.InvokeScriptTxValidator
import monix.eval.Coeval
import play.api.libs.json.*
import shapeless.*

object UtilsEvaluator {
  object ConflictingRequestStructure        extends ValidationError
  case class ParseJsonError(error: JsError) extends ValidationError

  case class EvaluateOptions(evaluateScriptComplexityLimit: Int, maxTxErrorLogSize: Int, enableTraces: Boolean, intAsString: Boolean)
  def compile(version: StdLibVersion)(str: String): Either[GenericError, EXPR] =
    ExpressionCompiler
      .compileUntyped(str, NoLibraries, utils.compilerContext(version, Expression, isAssetScript = false).copy(arbitraryDeclarations = true), version)
      .leftMap(GenericError(_))

  def evaluate(
      blockchain: Blockchain,
      dAppAddress: Address,
      request: JsObject,
      options: EvaluateOptions,
      wrapDAppEnv: DAppEnvironment => DAppEnvironmentInterface = identity
  ): JsObject =
    Evaluation
      .build(blockchain, dAppAddress, request)
      .map { case (evaluation, scriptInfo) => evaluate(evaluation, scriptInfo, dAppAddress, options, wrapDAppEnv) }
      .leftMap(validationErrorToJson(_, options.maxTxErrorLogSize))
      .merge

  def evaluate(
      evaluation: Evaluation,
      scriptInfo: AccountScriptInfo,
      dAppAddress: Address,
      options: EvaluateOptions,
      wrapDAppEnv: DAppEnvironment => DAppEnvironmentInterface
  ): JsObject = {
    val script = scriptInfo.script
    UtilsEvaluator
      .executeExpression(evaluation.blockchain, script, dAppAddress, scriptInfo.publicKey, options.evaluateScriptComplexityLimit)(
        evaluation.txLike,
        evaluation.dAppToExpr,
        wrapDAppEnv
      )
      .fold(
        validationErrorToJson(_, options.maxTxErrorLogSize),
        { r =>
          val traceObj = if (options.enableTraces) Json.obj(TraceStep.logJson(r.log)) else Json.obj()
          traceObj ++ Json.obj(
            "result"       -> ScriptValuesJson.serializeValue(r.result, options.intAsString),
            "complexity"   -> r.complexity,
            "stateChanges" -> r.scriptResult
          )
        }
      )
  }

  def validationErrorToJson(e: ValidationError, maxTxErrorLogSize: Int): JsObject = e match {
    case e: InvokeRejectError        => Json.obj("error" -> ScriptExecutionError.Id, "message" -> e.toStringWithLog(maxTxErrorLogSize))
    case ConflictingRequestStructure => ApiError.ConflictingRequestStructure.json
    case e: ParseJsonError           => ApiError.WrongJson(None, e.error.errors).json
    case e                           => ApiError.fromValidationError(e).json
  }

  case class ExecuteResult(result: EVALUATED, complexity: Int, log: Log[Id], scriptResult: InvokeScriptResult)

  def executeExpression(blockchain: Blockchain, script: Script, dAppAddress: Address, dAppPk: PublicKey, limit: Int)(
      invoke: InvokeScriptTransactionLike,
      dAppToExpr: DApp => Either[ValidationError, EXPR],
      wrapDAppEnv: DAppEnvironment => DAppEnvironmentInterface
  ): Either[ValidationError, ExecuteResult] =
    for {
      _  <- InvokeScriptTxValidator.checkAmounts(invoke.payments).toEither.leftMap(_.head)
      ds <- DirectiveSet(script.stdLibVersion, Account, DAppType).leftMap(GenericError(_))
      paymentsSnapshot <- InvokeDiffsCommon.paymentsPart(
        blockchain,
        invoke,
        dAppAddress,
        Map()
      )
      underlyingEnvironment =
        new DAppEnvironment(
          AddressScheme.current.chainId,
          Coeval.raiseError(new IllegalStateException("No input entity available")),
          Coeval.evalOnce(blockchain.height),
          blockchain,
          Coproduct[Tthis](Recipient.Address(ByteStr(dAppAddress.bytes))),
          ds,
          script.stdLibVersion,
          invoke,
          dAppAddress,
          dAppPk,
          Set.empty[Address],
          limitedExecution = false,
          enableExecutionLog = true,
          limit,
          remainingCalls = ContractLimits.MaxSyncDAppCalls(script.stdLibVersion),
          availableActions = ActionLimits(
            ContractLimits.MaxCallableActionsAmountBeforeV6(script.stdLibVersion),
            ContractLimits.MaxBalanceScriptActionsAmountV6,
            ContractLimits.MaxAssetScriptActionsAmountV6,
            ContractLimits.MaxWriteSetSize,
            ContractLimits.MaxTotalWriteSetSizeInBytes
          ),
          availablePayments = ContractLimits.MaxTotalPaymentAmountRideV6,
          currentSnapshot = paymentsSnapshot,
          invocationRoot = DAppEnvironment.InvocationTreeTracker(DAppEnvironment.DAppInvocation(dAppAddress, null, Nil)),
          wrapDAppEnv = wrapDAppEnv
        )
      environment = wrapDAppEnv(underlyingEnvironment)
      ctx         = BlockchainContext.build(ds, environment, fixUnicodeFunctions = true, useNewPowPrecision = true, fixBigScriptField = true)
      dApp        = ContractScriptCompactor.decompact(script.expr.asInstanceOf[DApp])
      expr <- dAppToExpr(dApp)
      limitedResult <- EvaluatorV2
        .applyLimitedCoeval(
          expr,
          LogExtraInfo(),
          limit,
          ctx,
          script.stdLibVersion,
          correctFunctionCallScope = blockchain.checkEstimatorSumOverflow,
          newMode = blockchain.newEvaluatorMode,
          checkConstructorArgsTypes = true,
          enableExecutionLog = true
        )
        .value()
        .leftMap { case (err, _, log) => InvokeRejectError(err.message, log) }
      (evaluated, usedComplexity, log) <- limitedResult match {
        case (eval: EVALUATED, unusedComplexity, log) => Right((eval, limit - unusedComplexity, log))
        case (_: EXPR, _, log)                        => Left(InvokeRejectError(s"Calculation complexity limit exceeded", log))
      }
      snapshot <- ScriptResult
        .fromObj(ctx, invoke.id(), evaluated, ds.stdLibVersion, unusedComplexity = 0)
        .bimap(
          _ => Right(StateSnapshot.empty),
          r =>
            InvokeDiffsCommon
              .processActions(
                StructuredCallableActions(r.actions, blockchain),
                ds.stdLibVersion,
                script.stdLibVersion,
                dAppAddress,
                dAppPk,
                usedComplexity,
                invoke,
                SnapshotBlockchain(blockchain, environment.currentSnapshot),
                System.currentTimeMillis(),
                isSyncCall = false,
                limitedExecution = false,
                limit,
                Nil,
                enableExecutionLog = true,
                log
              )
              .resultE
        )
        .merge
      totalDiff     = paymentsSnapshot |+| snapshot
      totalSnapshot = addWavesToDefaultInvoker(totalDiff, blockchain)
      _ <- TransactionDiffer.validateBalance(blockchain, InvokeScript, totalSnapshot)
      _ <- TransactionDiffer.assetsVerifierDiff(blockchain, invoke, verify = true, totalSnapshot, Int.MaxValue, enableExecutionLog = true).resultE
      rootScriptResult  = snapshot.scriptResults.headOption.map(_._2).getOrElse(InvokeScriptResult.empty)
      innerScriptResult = environment.currentSnapshot.scriptResults.values.fold(InvokeScriptResult.empty)(_ |+| _)
    } yield ExecuteResult(evaluated, usedComplexity, log, innerScriptResult |+| rootScriptResult)

  private def addWavesToDefaultInvoker(snapshot: StateSnapshot, blockchain: Blockchain) =
    if (snapshot.balances.get((UtilsApiRoute.DefaultAddress, Waves)).exists(_ >= Long.MaxValue / 10))
      snapshot
    else
      snapshot.addBalances(Map(UtilsApiRoute.DefaultAddress -> Portfolio.waves(Long.MaxValue / 10)), blockchain).explicitGet()
}

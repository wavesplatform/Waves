package com.wavesplatform.api.http.utils

import cats.Id
import cats.implicits.catsSyntaxSemigroup
import cats.syntax.either.*
import com.wavesplatform.account.{Address, AddressOrAlias, AddressScheme, PublicKey}
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
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, EXPR}
import com.wavesplatform.lang.v1.compiler.{ContractScriptCompactor, ExpressionCompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.{Invocation, LogExtraInfo}
import com.wavesplatform.lang.v1.evaluator.{EvaluatorV2, Log, ScriptResult}
import com.wavesplatform.lang.v1.parser.Parser.LibrariesOffset.NoLibraries
import com.wavesplatform.lang.v1.traits.Environment.Tthis
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader}
import com.wavesplatform.lang.{ValidationError, utils}
import com.wavesplatform.serialization.ScriptValuesJson
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, ScriptExtraFee}
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.state.diffs.invoke.{InvokeDiffsCommon, InvokeScriptTransactionLike, StructuredCallableActions}
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.state.{AccountScriptInfo, Blockchain, Diff, InvokeScriptResult, OverriddenBlockchain, Portfolio}
import com.wavesplatform.transaction.TransactionType.{InvokeScript, TransactionType}
import com.wavesplatform.transaction.TxValidationError.{GenericError, InvokeRejectError}
import com.wavesplatform.transaction.smart.*
import com.wavesplatform.transaction.smart.DAppEnvironment.ActionLimits
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.trace.TraceStep
import com.wavesplatform.transaction.validation.impl.InvokeScriptTxValidator
import com.wavesplatform.transaction.{Asset, TransactionType}
import monix.eval.Coeval
import play.api.libs.json.*
import shapeless.*

import scala.util.control.NoStackTrace

object UtilsEvaluator {
  def compile(version: StdLibVersion)(str: String): Either[GenericError, EXPR] =
    ExpressionCompiler
      .compileUntyped(str, NoLibraries, utils.compilerContext(version, Expression, isAssetScript = false).copy(arbitraryDeclarations = true))
      .leftMap(GenericError(_))

  def toInvokeScriptLike(invocation: Invocation, dAppAddress: Address) =
    new InvokeScriptTransactionLike {
      override def dApp: AddressOrAlias              = dAppAddress
      override def funcCall: Terms.FUNCTION_CALL     = invocation.funcCall
      override def root: InvokeScriptTransactionLike = this
      override val sender: PublicKey                 = PublicKey(invocation.callerPk)
      override def assetFee: (Asset, Long)           = (Asset.fromCompatId(invocation.feeAssetId), invocation.fee)
      override def timestamp: Long                   = System.currentTimeMillis()
      override def chainId: Byte                     = AddressScheme.current.chainId
      override def id: Coeval[ByteStr]               = Coeval(invocation.transactionId)
      override val tpe: TransactionType              = TransactionType.InvokeScript
      override def payments: Seq[Payment] =
        invocation.payments.payments.map { case (amount, assetId) =>
          Payment(amount, Asset.fromCompatId(assetId))
        }
    }

  def emptyInvokeScriptLike(dAppAddress: Address) =
    new InvokeScriptTransactionLike {
      override def dApp: AddressOrAlias              = dAppAddress
      override def funcCall: Terms.FUNCTION_CALL     = Terms.FUNCTION_CALL(FunctionHeader.User(""), Nil)
      override def payments: Seq[Payment]            = Seq.empty
      override def root: InvokeScriptTransactionLike = this
      override val sender: PublicKey                 = PublicKey(ByteStr(new Array[Byte](32)))
      override def assetFee: (Asset, Long)           = Asset.Waves -> FeeConstants(InvokeScript) * ScriptExtraFee
      override def timestamp: Long                   = System.currentTimeMillis()
      override def chainId: Byte                     = AddressScheme.current.chainId
      override def id: Coeval[ByteStr]               = Coeval.evalOnce(ByteStr.empty)
      override val tpe: TransactionType              = TransactionType.InvokeScript
    }

  object NoRequestStructure                 extends ValidationError
  object ConflictingRequestStructure        extends ValidationError
  case class ParseJsonError(error: JsError) extends ValidationError

  def evaluate(
      evaluateScriptComplexityLimit: Int,
      blockchain: Blockchain,
      address: Address,
      request: JsObject,
      trace: Boolean,
      maxTxErrorLogSize: Int,
      intAsString: Boolean,
      wrapDAppEnv: DAppEnvironment => DAppEnvironmentInterface = identity
  ): JsObject = {
    val r: Either[ValidationError, JsObject] = for {
      evRequest <- parseEvaluateRequest(request)
      overridden = evRequest.state.foldLeft(blockchain)(new OverriddenBlockchain(_, _))
      scriptInfo <- overridden.accountScript(address) match {
        case Some(x) => x.asRight
        case None    => GenericError(s"There is no script on '$address'").asLeft
      }
      evaluation <- evRequest match {
        case evRequest: UtilsExprRequest =>
          evRequest.parseCall(scriptInfo.script.stdLibVersion).map { expr =>
            ExprEvaluation(expr, UtilsEvaluator.emptyInvokeScriptLike(address))
          }
        case evRequest: UtilsInvocationRequest =>
          evRequest.toInvocation.map { invocation =>
            InvocationEvaluation(invocation, UtilsEvaluator.toInvokeScriptLike(invocation, address))
          }

        case _ => throw new RuntimeException("Impossible")
      }
    } yield evaluate(
      evaluateScriptComplexityLimit,
      overridden,
      scriptInfo,
      evaluation,
      address,
      trace,
      maxTxErrorLogSize,
      intAsString,
      wrapDAppEnv
    )

    r.leftMap(validationErrorToJson(_, maxTxErrorLogSize)).merge
  }

  def evaluate(
      evaluateScriptComplexityLimit: Int,
      blockchain: Blockchain,
      scriptInfo: AccountScriptInfo,
      evaluation: Evaluation,
      address: Address,
      trace: Boolean,
      maxTxErrorLogSize: Int,
      intAsString: Boolean,
      wrapDAppEnv: DAppEnvironment => DAppEnvironmentInterface
  ): JsObject = {
    val script = scriptInfo.script
    UtilsEvaluator
      .executeExpression(blockchain, script, address, scriptInfo.publicKey, evaluateScriptComplexityLimit)(
        evaluation.txLike,
        evaluation.dAppToExpr(_, script),
        wrapDAppEnv
      )
      .fold(
        validationErrorToJson(_, maxTxErrorLogSize),
        { r =>
          val traceObj = if (trace) Json.obj(TraceStep.logJson(r.log)) else Json.obj()
          traceObj ++ Json.obj(
            "result"       -> ScriptValuesJson.serializeValue(r.result, intAsString),
            "complexity"   -> r.complexity,
            "stateChanges" -> r.scriptResult
          )
        }
      )
  }

  def validationErrorToJson(e: ValidationError, maxTxErrorLogSize: Int): JsObject = e match {
    case e: InvokeRejectError => Json.obj("error" -> ScriptExecutionError.Id, "message" -> e.toStringWithLog(maxTxErrorLogSize))
    case NoRequestStructure =>
      ApiError
        .WrongJson(cause = Some(new IllegalArgumentException() with NoStackTrace {
          override def toString: String = "Either expression or invocation structure is required in request"
        }))
        .json
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
      _            <- InvokeScriptTxValidator.checkAmounts(invoke.payments).toEither.leftMap(_.head)
      ds           <- DirectiveSet(script.stdLibVersion, Account, DAppType).leftMap(GenericError(_))
      paymentsDiff <- InvokeDiffsCommon.paymentsPart(invoke, dAppAddress, Map())
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
          currentDiff = paymentsDiff,
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
          checkConstructorArgsTypes = true
        )
        .value()
        .leftMap { case (err, _, log) => InvokeRejectError(err.message, log) }
      (evaluated, usedComplexity, log) <- limitedResult match {
        case (eval: EVALUATED, unusedComplexity, log) => Right((eval, limit - unusedComplexity, log))
        case (_: EXPR, _, log)                        => Left(InvokeRejectError(s"Calculation complexity limit exceeded", log))
      }
      diff <- ScriptResult
        .fromObj(ctx, invoke.id(), evaluated, ds.stdLibVersion, unusedComplexity = 0)
        .bimap(
          _ => Right(Diff.empty),
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
                CompositeBlockchain(blockchain, paymentsDiff),
                System.currentTimeMillis(),
                isSyncCall = false,
                limitedExecution = false,
                limit,
                Nil,
                log
              )
              .resultE
        )
        .merge
      totalDiff <- diff.combineE(paymentsDiff)
      _         <- TransactionDiffer.validateBalance(blockchain, InvokeScript, addWavesToDefaultInvoker(totalDiff))
      _         <- TransactionDiffer.assetsVerifierDiff(blockchain, invoke, verify = true, totalDiff, Int.MaxValue).resultE
      rootScriptResult  = diff.scriptResults.headOption.map(_._2).getOrElse(InvokeScriptResult.empty)
      innerScriptResult = underlyingEnvironment.currentDiff.scriptResults.values.fold(InvokeScriptResult.empty)(_ |+| _)
    } yield ExecuteResult(evaluated, usedComplexity, log, innerScriptResult |+| rootScriptResult)

  private def addWavesToDefaultInvoker(diff: Diff) =
    if (diff.portfolios.get(UtilsApiRoute.DefaultAddress).exists(_.balance >= Long.MaxValue / 10))
      diff
    else
      diff.combineE(Diff(Map(UtilsApiRoute.DefaultAddress -> Portfolio.waves(Long.MaxValue / 10)))).explicitGet()
}

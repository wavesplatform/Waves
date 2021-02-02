package com.wavesplatform.state.diffs.invoke

import cats.Id
import cats.implicits._
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.EstimatorProvider._
import com.wavesplatform.lang._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext, unit}
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, LazyVal}
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, IncompleteResult, Log, ScriptResult, ScriptResultV3, ScriptResultV4}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.Tx.ScriptTransfer
import com.wavesplatform.lang.v1.traits.domain._
import com.wavesplatform.metrics._
import com.wavesplatform.state._
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.{AssetVerifierTrace, TraceStep, TracedResult}
import com.wavesplatform.transaction.smart.{DApp => DAppTarget, _}
import com.wavesplatform.transaction.{Transaction, TxValidationError}
import monix.eval.Coeval
import shapeless.Coproduct

import scala.util.Right

object InvokeScriptDiff {
  private val stats = TxProcessingStats
  import stats.TxTimerExt

  case class CoevalET[+A](v: Coeval[TracedResult[ValidationError, A]]) extends AnyVal {
    def flatMap[B](f: A => CoevalET[B]): CoevalET[B] = {
      val r = v.flatMap(
        t =>
          t.resultE match {
            case Right(value)  => f(value).v
            case l: Left[_, _] => Coeval.now(TracedResult(l.asInstanceOf[Either[ValidationError, B]]))
          }
      )
      CoevalET(r)
    }

    def map[B](f: A => B): CoevalET[B] =
      CoevalET(v.map(_.map(f)))

    def withFilter(f: A => Boolean): CoevalET[A] =
      CoevalET(v.map(_.withFilter(f)))
  }

  def traced[A](a: Either[ValidationError, A], trace: List[TraceStep] = Nil): CoevalET[A] =
    CoevalET(Coeval.now(TracedResult(a, trace)))

  def apply(blockchain: Blockchain, blockTime: Long, limitedExecution: Boolean, remainingComplexity: Long)(
      tx: InvokeScript
  ): CoevalET[(Diff, EVALUATED)] = {
    if (remainingComplexity <= 0) {
      return traced(Left(ValidationError.ScriptRunsLimitError(s"Too many scripts run while invoke $tx, maybe not enough fee.")))
    }
    val dAppAddress  = tx.dAppAddress
    val accScript    = blockchain.accountScript(dAppAddress)
    val functionCall = tx.funcCall

    accScript match {
      case Some(AccountScriptInfo(pk, ContractScriptImpl(version, contract), _, callableComplexities)) =>
        for {
          invocationComplexity <- traced {
            InvokeDiffsCommon.getInvocationComplexity(blockchain, tx.funcCall, callableComplexities, dAppAddress)
          }

          _ <- traced(
            Either.cond(
              invocationComplexity <= ContractLimits.MaxComplexityByVersion(version),
              (),
              GenericError("Continuation is not allowed for Invoke by script")
            )
          )

          directives: DirectiveSet <- traced(DirectiveSet(version, Account, DAppType).leftMap(GenericError.apply))
          payments                 <- traced(AttachedPaymentExtractor.extractPayments(tx, version, blockchain, DAppTarget).leftMap(GenericError.apply))
          checkedPayments = payments.payments.flatMap {
            case (amount, Some(assetId)) => blockchain.assetScript(IssuedAsset(assetId)).flatMap(s => Some((s, amount, assetId)))
            case _                       => None
          }
          _ <- checkedPayments.foldLeft(traced(Right(()): TxValidationError.Validation[Unit])) { (prev, a) =>
            (prev.v.value(), a) match {
              case (TracedResult(Left(_), _), _) => prev
              case (_, (script, amount, assetId)) =>
                val pseudoTx: PseudoTx = ScriptTransfer(
                  Some(assetId),
                  Recipient.Address(ByteStr(tx.senderDApp.bytes)),
                  tx.sender,
                  Recipient.Address(ByteStr(tx.dAppAddress.bytes)),
                  amount,
                  tx.root.timestamp,
                  tx.root.id()
                )
                val r = ScriptRunner(
                  Coproduct[TxOrd](pseudoTx),
                  blockchain,
                  script.script,
                  isAssetScript = true,
                  scriptContainerAddress = Coproduct[Environment.Tthis](Environment.AssetId(assetId.arr)),
                  Int.MaxValue
                ) match {
                  case (log, Left(error)) =>
                    val err = FailedTransactionError.assetExecutionInAction(error, 0, log, assetId)
                    TracedResult(Left(err), List(AssetVerifierTrace(assetId, Some(err))))
                  case (log, Right(FALSE)) =>
                    val err = FailedTransactionError.notAllowedByAssetInAction(0, log, assetId)
                    TracedResult(Left(err), List(AssetVerifierTrace(assetId, Some(err))))
                  case (_, Right(TRUE)) =>
                    TracedResult(Right(()))
                  case (log, Right(x)) =>
                    val err = FailedTransactionError.assetExecutionInAction(s"Script returned not a boolean result, but $x", 0, log, assetId)
                    TracedResult(Left(err), List(AssetVerifierTrace(assetId, Some(err))))
                }
                CoevalET(Coeval.now(r))
            }
          }
          tthis = Coproduct[Environment.Tthis](Recipient.Address(ByteStr(dAppAddress.bytes)))
          input <- traced(
            buildThisValue(Coproduct[TxOrd](tx.root: Transaction), blockchain, directives, tthis).leftMap(GenericError.apply)
          )

          result <- for {
            scriptResult <- {
              val scriptResultE = stats.invokedScriptExecution.measureForType(InvokeScriptTransaction.typeId)({
                val invoker = tx.senderDApp
                val invocation = ContractEvaluator.Invocation(
                  functionCall,
                  Recipient.Address(ByteStr(invoker.bytes)),
                  ByteStr(tx.sender.arr),
                  payments,
                  tx.root.id(),
                  tx.root.fee,
                  tx.root.feeAssetId.compatId
                )
                val height = blockchain.height

                val environment = new DAppEnvironment(
                  AddressScheme.current.chainId,
                  Coeval.evalOnce(input),
                  Coeval(height),
                  blockchain,
                  tthis,
                  directives,
                  tx.root,
                  tx.dAppAddress,
                  pk,
                  tx.senderDApp,
                  remainingComplexity - invocationComplexity - checkedPayments.map(_._1.complexity).sum
                )

                //to avoid continuations when evaluating underestimated by EstimatorV2 scripts
                val fullLimit =
                  if (blockchain.estimator == ScriptEstimatorV2)
                    Int.MaxValue
                  else
                    ContractLimits.MaxComplexityByVersion(version)

                val failFreeLimit =
                  if (blockchain.isFeatureActivated(BlockchainFeatures.BlockV5))
                    ContractLimits.FailFreeInvokeComplexity
                  else
                    fullLimit

                for {
                  (failFreeResult, evaluationCtx, failFreeLog) <- CoevalET(
                    evaluateV2(version, contract, directives, invocation, environment, failFreeLimit).map(TracedResult(_))
                  )
                  (result, log) <- failFreeResult match {
                    case IncompleteResult(expr, unusedComplexity) if !limitedExecution =>
                      CoevalET(
                        continueEvaluation(
                          version,
                          expr,
                          evaluationCtx,
                          fullLimit - failFreeLimit + unusedComplexity,
                          tx.root.id(),
                          invocationComplexity
                        ).map(TracedResult(_))
                      )
                    case _ =>
                      traced(Right((failFreeResult, Nil)))
                  }
                } yield (environment.currentDiff, (result, failFreeLog ::: log))
              })
              /*              traced(
                scriptResultE,
                List(
                  InvokeScriptTrace(tx.root.id.value(), tx.dAppAddress, functionCall, scriptResultE.map(_._2._1), scriptResultE.fold(_.log, _._2._2))
                )
              )*/
              scriptResultE
            }

            doProcessActions = (actions: List[CallableAction]) =>
              CoevalET(
                Coeval.now(
                  InvokeDiffsCommon.processActions(
                    actions,
                    version,
                    dAppAddress,
                    pk,
                    invocationComplexity,
                    tx,
                    CompositeBlockchain(blockchain, Some(scriptResult._1)),
                    blockTime,
                    remainingComplexity - scriptResult._1.scriptsComplexity - checkedPayments.map(_._1.complexity).sum,
                    isSyncCall = true,
              limitedExecution,
              Seq()
            ))
              )

            resultDiff <- scriptResult._2._1 match {
              case ScriptResultV3(dataItems, transfers, _) => doProcessActions(dataItems ::: transfers).map(r => (r, unit))
              case ScriptResultV4(actions, _, ret)         => doProcessActions(actions).map(r => (r, ret))
              case _: IncompleteResult                     => traced(Left(GenericError("Unexpected IncompleteResult")))
            }
          } yield resultDiff.copy(_1 = scriptResult._1 combine resultDiff._1)
        } yield result

      case _ => traced(Left(GenericError(s"No contract at address ${tx.dAppAddress}")))
    }
  }

  private def evaluateV2(
      version: StdLibVersion,
      contract: DApp,
      directives: DirectiveSet,
      invocation: ContractEvaluator.Invocation,
      environment: Environment[Id],
      limit: Int
  ): Coeval[Either[ScriptExecutionError, (ScriptResult, EvaluationContext[Environment, Id], Log[Id])]] = {
    val wavesContext = WavesContext.build(directives)
    val ctx =
      PureContext.build(version).withEnvironment[Environment] |+|
        CryptoContext.build(Global, version).withEnvironment[Environment] |+|
        wavesContext.copy(vars = Map())

    val freezingLets  = wavesContext.evaluationContext(environment).letDefs
    val evaluationCtx = ctx.evaluationContext(environment)

    ContractEvaluator
      .applyV2Coeval(evaluationCtx, freezingLets, contract, invocation, version, limit)
      .map(
        _.bimap(
          { case (error, log) => ScriptExecutionError.dAppExecution(error, log) }, { case (result, log) => (result, evaluationCtx, log) }
        )
      )
  }

  private def continueEvaluation(
      version: StdLibVersion,
      expr: EXPR,
      evaluationCtx: EvaluationContext[Environment, Id],
      limit: Int,
      transactionId: ByteStr,
      failComplexity: Long
  ): Coeval[Either[FailedTransactionError, (ScriptResult, Log[Id])]] =
    ContractEvaluator
      .applyV2Coeval(evaluationCtx, Map[String, LazyVal[Id]](), expr, version, transactionId, limit)
      .map(
        _.leftMap(
          { case (error, log) => FailedTransactionError.dAppExecution(error, failComplexity, log) }
        )
      )
}

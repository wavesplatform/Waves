package com.wavesplatform.state.diffs.invoke

import cats.Id
import cats.implicits._
import com.google.common.base.Throwables
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
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
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{unit, CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, LazyVal}
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, IncompleteResult, Log, ScriptResult, ScriptResultV3, ScriptResultV4}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain._
import com.wavesplatform.lang.v1.traits.domain.Tx.ScriptTransfer
import com.wavesplatform.metrics._
import com.wavesplatform.state._
import com.wavesplatform.state.reader.CompositeBlockchain
//import com.wavesplatform.state.InvokeScriptResult.Payment
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.{InvokeScriptTrace, TracedResult, AssetVerifierTrace}
import com.wavesplatform.transaction.smart.{DApp => DAppTarget, _}
import com.wavesplatform.transaction.Asset.IssuedAsset
import monix.eval.Coeval
import shapeless.Coproduct

import scala.util.{Right, Try, Failure, Success}

object InvokeScriptDiff {

  private val stats = TxProcessingStats
  import stats.TxTimerExt

  def apply(blockchain: Blockchain, blockTime: Long, limitedExecution: Boolean, runsLimit: Int, invokeDeep: Int)(tx: InvokeScript): TracedResult[ValidationError, (Diff, EVALUATED)] = {
    if(runsLimit <= 0 || invokeDeep <= 0) return TracedResult(Left(ValidationError.ScriptRunsLimitError(s"Too many scripts run while invoke $tx, maybe not enough fee.")))
    val dAppAddress = tx.dAppAddress
    val accScript = blockchain.accountScript(dAppAddress)
    val functionCall  = tx.funcCall

    accScript match {
      case Some(AccountScriptInfo(pk, ContractScriptImpl(version, contract), _, callableComplexities)) =>
        for {
          invocationComplexity <- TracedResult {
            InvokeDiffsCommon.getInvocationComplexity(blockchain, tx.funcCall, callableComplexities, dAppAddress)
          }

          directives <- TracedResult.wrapE(DirectiveSet(version, Account, DAppType).leftMap(GenericError.apply))
          payments   <- TracedResult.wrapE(AttachedPaymentExtractor.extractPayments(tx, version, blockchain, DAppTarget).leftMap(GenericError.apply))
          checkedPayments = payments.payments.flatMap {
            case (amount, Some(assetId)) => blockchain.assetScript(IssuedAsset(assetId)).flatMap(s => Some((s.script, amount, assetId)))
            case _ => None
          }
          _ <- checkedPayments.foldLeft(TracedResult(Right(()):TxValidationError.Validation[Unit])) { (prev, a) => (prev, a) match {
              case (TracedResult(Left(_), _), _) => prev
              case (_, (script, amount, assetId)) =>
                val pseudoTx:PseudoTx = ScriptTransfer(
                  Some(assetId),
                  Recipient.Address(ByteStr(tx.senderDApp.bytes)),
                  tx.sender,
                  Recipient.Address(ByteStr(tx.dAppAddress.bytes)),
                  amount,
                  tx.root.timestamp,
                  tx.root.id()
                )
                Try {
                  ScriptRunner(
                    Coproduct[TxOrd](pseudoTx),
                    blockchain,
                    script,
                    isAssetScript = true,
                    scriptContainerAddress = Coproduct[Environment.Tthis](Environment.AssetId(assetId.arr)),
                    Int.MaxValue
                  )  match {
                     case (log, Left(error))  =>
                       val err = FailedTransactionError.assetExecutionInAction(error, 0, log, assetId)
                       TracedResult(Left(err), List(AssetVerifierTrace(assetId, Some(err))))
                     case (log, Right(FALSE)) =>
                       val err = FailedTransactionError.notAllowedByAssetInAction(0, log, assetId)
                       TracedResult(Left(err), List(AssetVerifierTrace(assetId, Some(err))))
                     case (log, Right(TRUE))  => TracedResult(Right(()))
                     case (log, Right(x)) =>
                       val err = FailedTransactionError.assetExecutionInAction(s"Script returned not a boolean result, but $x", 0, log, assetId)
                       TracedResult(Left(err), List(AssetVerifierTrace(assetId, Some(err))))
                   }
                } match {
                  case Failure(e) =>
                    TracedResult(Left(
                      FailedTransactionError
                        .assetExecutionInAction(s"Uncaught execution error: ${Throwables.getStackTraceAsString(e)}", 0L, List.empty, assetId)
                    ))
                  case Success(s) => s
                }
              }
          }
          tthis = Coproduct[Environment.Tthis](Recipient.Address(ByteStr(dAppAddress.bytes)))
          input <- TracedResult.wrapE(buildThisValue(Coproduct[TxOrd](tx.root: Transaction), blockchain, directives, tthis).leftMap(GenericError.apply))

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
                  runsLimit-1-checkedPayments.size,
                  invokeDeep-1
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
                  (failFreeResult, evaluationCtx, failFreeLog) <- evaluateV2(version, contract, directives, invocation, environment, failFreeLimit)
                  (result, log) <- failFreeResult match {
                    case IncompleteResult(expr, unusedComplexity) if !limitedExecution =>
                      continueEvaluation(version, expr, evaluationCtx, fullLimit - failFreeLimit + unusedComplexity, tx.root.id(), invocationComplexity)
                    case _ =>
                      Right((failFreeResult, Nil))
                  }
                } yield (environment.currentDiff, (result, failFreeLog ::: log))
              })
              TracedResult(
                scriptResultE,
                List(InvokeScriptTrace(tx.dAppAddress, functionCall, scriptResultE.map(_._2._1), scriptResultE.fold(_.log, _._2._2)))
              )
            }

            doProcessActions = InvokeDiffsCommon.processActions(
              _,
              version,
              dAppAddress,
              pk,
              None,
              invocationComplexity,
              tx,
              CompositeBlockchain(blockchain, Some(scriptResult._1)),
              blockTime,
              runsLimit - scriptResult._1.scriptsRun-checkedPayments.size,
              limitedExecution
            )

            resultDiff <- scriptResult._2._1 match {
              case ScriptResultV3(dataItems, transfers)    => doProcessActions(dataItems ::: transfers).map(r => (r, unit))
              case ScriptResultV4(actions, ret)            => doProcessActions(actions).map(r => (r, ret))
              case _: IncompleteResult                     => TracedResult(Left(GenericError("Unexpected IncompleteResult")))
            }
          } yield resultDiff.copy(_1 = scriptResult._1 combine resultDiff._1)
        } yield result

      case _       => TracedResult(Left(GenericError(s"No contract at address ${tx.dAppAddress}")))
    }
  }

  private def evaluateV2(
      version: StdLibVersion,
      contract: DApp,
      directives: DirectiveSet,
      invocation: ContractEvaluator.Invocation,
      environment: Environment[Id],
      limit: Int
  ): Either[ScriptExecutionError, (ScriptResult, EvaluationContext[Environment, Id], Log[Id])] = {
    val wavesContext = WavesContext.build(directives)
    val ctx =
      PureContext.build(version).withEnvironment[Environment] |+|
        CryptoContext.build(Global, version).withEnvironment[Environment] |+|
        wavesContext.copy(vars = Map())

    val freezingLets  = wavesContext.evaluationContext(environment).letDefs
    val evaluationCtx = ctx.evaluationContext(environment)

    Try(ContractEvaluator.applyV2(evaluationCtx, freezingLets, contract, invocation, version, limit))
      .fold(
        e => Left((e.getMessage, Nil)),
        _.map { case (result, log) => (result, evaluationCtx, log) }
      )
      .leftMap {
        case (error, log) => ScriptExecutionError.dAppExecution(error, log)
      }
  }

  private def continueEvaluation(
      version: StdLibVersion,
      expr: EXPR,
      evaluationCtx: EvaluationContext[Environment, Id],
      limit: Int,
      transactionId: ByteStr,
      failComplexity: Long
  ): Either[FailedTransactionError, (ScriptResult, Log[Id])] =
    Try(ContractEvaluator.applyV2(evaluationCtx, Map[String, LazyVal[Id]](), expr, version, transactionId, limit))
      .fold(e => Left((e.getMessage, Nil)), identity)
      .leftMap { case (error, log) => FailedTransactionError.dAppExecution(error, failComplexity, log) }

}

package com.wavesplatform.state.diffs.invoke

import cats.Id
import cats.implicits._
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.EstimatorProvider._
import com.wavesplatform.features.FunctionCallPolicyProvider._
import com.wavesplatform.lang._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.ContractCompiler
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, LazyVal}
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, IncompleteResult, Log, ScriptResult, ScriptResultV3, ScriptResultV4}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain._
import com.wavesplatform.metrics._
import com.wavesplatform.state._
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.{InvokeScriptTrace, TracedResult}
import com.wavesplatform.transaction.smart.{DApp => DAppTarget, _}
import monix.eval.Coeval
import shapeless.Coproduct

import scala.util.{Right, Try}

object InvokeScriptTransactionDiff {

  private val stats = TxProcessingStats
  import stats.TxTimerExt

  def apply(blockchain: Blockchain, blockTime: Long, limitedExecution: Boolean)(tx: InvokeScriptTransaction): TracedResult[ValidationError, Diff] = {

    val dAppAddressEi = blockchain.resolveAlias(tx.dAppAddressOrAlias)
    val accScriptEi   = dAppAddressEi.map(blockchain.accountScript)
    val functionCall  = tx.funcCall

    accScriptEi match {
      case Right(Some(AccountScriptInfo(pk, ContractScriptImpl(version, contract), _, callableComplexities))) =>
        for {
          _           <- TracedResult.wrapE(checkCall(functionCall, blockchain).leftMap(GenericError.apply))
          dAppAddress <- TracedResult(dAppAddressEi)

          directives <- TracedResult.wrapE(DirectiveSet(version, Account, DAppType).leftMap(GenericError.apply))
          payments   <- TracedResult.wrapE(AttachedPaymentExtractor.extractPayments(tx, version, blockchain, DAppTarget).leftMap(GenericError.apply))
          tthis = Coproduct[Environment.Tthis](Recipient.Address(ByteStr(dAppAddress.bytes)))
          input <- TracedResult.wrapE(buildThisValue(Coproduct[TxOrd](tx: Transaction), blockchain, directives, tthis).leftMap(GenericError.apply))

          invocationComplexity <- TracedResult {
            InvokeDiffsCommon.getInvocationComplexity(blockchain, tx, callableComplexities, dAppAddress)
          }

          stepLimit = ContractLimits.MaxComplexityByVersion(version)
          _ <- InvokeDiffsCommon.calcAndCheckFee(
            (message, _) => GenericError(message),
            tx,
            blockchain,
            stepLimit,
            invocationComplexity,
            issueList = Nil,
            actionScriptsInvoked = 0
          )

          result <- for {
            scriptResult <- {
              val scriptResultE = stats.invokedScriptExecution.measureForType(InvokeScriptTransaction.typeId)({
                val invoker = tx.sender.toAddress
                val invocation = ContractEvaluator.Invocation(
                  functionCall,
                  Recipient.Address(ByteStr(invoker.bytes)),
                  tx.sender,
                  payments,
                  ByteStr(tx.dAppAddressOrAlias.bytes),
                  tx.id(),
                  tx.fee,
                  tx.feeAssetId.compatId
                )
                val height = blockchain.height

                val disableThisWhenCalledByAlias = tx.dAppAddressOrAlias match {
                  case _: Alias if AddressScheme.current.chainId == 'T'.toByte =>
                    !blockchain.isFeatureActivated(BlockchainFeatures.BlockV5) && blockchain.height > 1100000
                  case _: Alias =>
                    !blockchain.isFeatureActivated(BlockchainFeatures.BlockV5)
                  case _ => false
                }

                val environment = new WavesEnvironment(
                  AddressScheme.current.chainId,
                  Coeval.evalOnce(input),
                  Coeval(height),
                  blockchain,
                  tthis,
                  directives,
                  tx.id(),
                  disableThisWhenCalledByAlias
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
                  (failFreeResult, evaluationCtx, failFreeLog) <- evaluateV2(
                    version,
                    contract,
                    directives,
                    invocation,
                    environment,
                    failFreeLimit,
                    continuationFirstStepMode = invocationComplexity > stepLimit
                  )
                  (result, log) <- failFreeResult match {
                    case IncompleteResult(expr, unusedComplexity) if !limitedExecution =>
                      continueEvaluation(
                        version,
                        expr,
                        evaluationCtx,
                        fullLimit - failFreeLimit + unusedComplexity,
                        tx.id(),
                        invocationComplexity,
                        continuationFirstStepMode = invocationComplexity > stepLimit
                      )
                    case _ =>
                      Right((failFreeResult, Nil))
                  }
                } yield (result, failFreeLog ::: log)
              })
              TracedResult(
                scriptResultE,
                List(InvokeScriptTrace(tx.dAppAddressOrAlias, functionCall, scriptResultE.map(_._1), scriptResultE.fold(_.log, _._2)))
              )
            }

            doProcessActions = InvokeDiffsCommon.processActions(
              _,
              version,
              dAppAddress,
              pk,
              invocationComplexity,
              tx,
              blockchain,
              blockTime,
              limitedExecution
            )

            resultDiff <- scriptResult._1 match {
              case ScriptResultV3(dataItems, transfers) => doProcessActions(dataItems ::: transfers)
              case ScriptResultV4(actions)              => doProcessActions(actions)
              case ir: IncompleteResult =>
                val state = ContinuationState.InProgress(nonce = 0, ir.expr, residualComplexity = ir.unusedComplexity, tx.id.value())
                TracedResult.wrapValue(Diff(tx = tx, continuationStates = Map(tx.id.value() -> state)))
            }
          } yield resultDiff
        } yield result

      case Left(l) => TracedResult(Left(l))
      case _       => TracedResult(Left(GenericError(s"No contract at address ${tx.dAppAddressOrAlias}")))
    }
  }

  private def evaluateV2(
      version: StdLibVersion,
      contract: DApp,
      directives: DirectiveSet,
      invocation: ContractEvaluator.Invocation,
      environment: WavesEnvironment,
      limit: Int,
      continuationFirstStepMode: Boolean
  ): Either[ScriptExecutionError, (ScriptResult, EvaluationContext[Environment, Id], Log[Id])] = {
    val wavesContext = WavesContext.build(directives)
    val ctx =
      PureContext.build(version).withEnvironment[Environment] |+|
        CryptoContext.build(Global, version).withEnvironment[Environment] |+|
        wavesContext.copy(vars = Map())

    val freezingLets  = wavesContext.evaluationContext(environment).letDefs
    val evaluationCtx = ctx.evaluationContext(environment)

    Try(ContractEvaluator.applyV2(evaluationCtx, freezingLets, contract, invocation, version, limit, continuationFirstStepMode))
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
      failComplexity: Long,
      continuationFirstStepMode: Boolean
  ): Either[FailedTransactionError, (ScriptResult, Log[Id])] =
    Try(ContractEvaluator.applyV2(evaluationCtx, Map[String, LazyVal[Id]](), expr, version, transactionId, limit, continuationFirstStepMode))
      .fold(e => Left((e.getMessage, Nil)), identity)
      .leftMap { case (error, log) => FailedTransactionError.dAppExecution(error, failComplexity, log) }

  private def checkCall(fc: FUNCTION_CALL, blockchain: Blockchain): Either[ExecutionError, Unit] = {
    val (check, expectedTypes) =
      if (blockchain.callableListArgumentsAllowed)
        (
          fc.args.forall(arg => arg.isInstanceOf[EVALUATED] && !arg.isInstanceOf[CaseObj]),
          ContractCompiler.allowedCallableTypesV4
        )
      else
        (
          fc.args.forall(arg => arg.isInstanceOf[EVALUATED] && !arg.isInstanceOf[CaseObj] && !arg.isInstanceOf[ARR]),
          ContractCompiler.primitiveCallableTypes
        )
    Either.cond(
      check,
      (),
      s"All arguments of InvokeScript must be one of the types: ${expectedTypes.mkString(", ")}"
    )
  }

}

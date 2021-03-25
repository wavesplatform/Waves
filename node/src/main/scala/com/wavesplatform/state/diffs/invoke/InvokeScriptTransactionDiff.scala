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
import com.wavesplatform.lang.v1.evaluator._
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain._
import com.wavesplatform.metrics._
import com.wavesplatform.state._
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.{InvokeScriptTrace, TracedResult}
import com.wavesplatform.transaction.smart.{DApp => DAppTarget, _}
import monix.eval.Coeval
import shapeless.Coproduct

import scala.util.Right

object InvokeScriptTransactionDiff {

  private def allIssues(r: InvokeScriptResult): Seq[Issue] = {
    r.issues ++ r.invokes.flatMap(s => allIssues(s.stateChanges))
  }

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

          invocationComplexity <- TracedResult {
            InvokeDiffsCommon.getInvocationComplexity(blockchain, tx.funcCall, callableComplexities, dAppAddress)
          }

          stepLimit = ContractLimits.MaxComplexityByVersion(version)

          fixedInvocationComplexity =
            if (blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls) && callableComplexities.contains(ScriptEstimatorV2.version))
              Math.min(invocationComplexity, stepLimit)
            else
              invocationComplexity

          (feeInWaves, _) <- InvokeDiffsCommon.calcAndCheckFee(
            (message, _) => GenericError(message),
            tx,
            blockchain,
            stepLimit,
            fixedInvocationComplexity,
            issueList = Nil,
            additionalScriptsInvoked = 0
          )

          directives <- TracedResult.wrapE(DirectiveSet(version, Account, DAppType).leftMap(GenericError.apply))
          payments   <- TracedResult.wrapE(AttachedPaymentExtractor.extractPayments(tx, version, blockchain, DAppTarget).leftMap(GenericError.apply))
          tthis = Coproduct[Environment.Tthis](Recipient.Address(ByteStr(dAppAddress.bytes)))
          input <- TracedResult.wrapE(buildThisValue(Coproduct[TxOrd](tx: Transaction), blockchain, directives, tthis).leftMap(GenericError.apply))

          result <- for {
            (invocationDiff, scriptResult, _) <- {
              val scriptResultE = stats.invokedScriptExecution.measureForType(InvokeScriptTransaction.typeId)({
                val invoker = tx.sender.toAddress
                val invocation = ContractEvaluator.Invocation(
                  functionCall,
                  Recipient.Address(ByteStr(invoker.bytes)),
                  tx.sender,
                  Recipient.Address(ByteStr(invoker.bytes)),
                  tx.sender,
                  payments,
                  tx.id(),
                  tx.fee,
                  tx.feeAssetId.compatId
                )
                val height = blockchain.height
                val remainingCalls = ContractLimits.MaxSyncDAppCalls(version)

                val environment = new DAppEnvironment(
                  AddressScheme.current.chainId,
                  Coeval.evalOnce(input),
                  Coeval(height),
                  blockchain,
                  tthis,
                  directives,
                  Some(tx),
                  dAppAddress,
                  pk,
                  dAppAddress,
                  Set(tx.senderAddress, dAppAddress),
                  remainingCalls,
                  (if (version < V5) {
                     Diff.empty
                   } else {
                     InvokeDiffsCommon.paymentsPart(tx, dAppAddress, Map())
                   })
                )

                val fullLimit =
                  if (blockchain.estimator == ScriptEstimatorV2)
                    Int.MaxValue //to avoid continuations when evaluating underestimated by EstimatorV2 scripts
                  else if (limitedExecution)
                    ContractLimits.FailFreeInvokeComplexity
                  else
                    ContractLimits.MaxTotalInvokeComplexity(version)

                val failFreeLimit =
                  if (blockchain.isFeatureActivated(BlockchainFeatures.BlockV5))
                    ContractLimits.FailFreeInvokeComplexity
                  else
                    fullLimit

                val paymentsComplexity = tx.checkedAssets.flatMap(blockchain.assetScript).map(_.complexity).sum

                for {
                  (result, log) <- evaluateV2(
                    version,
                    contract,
                    invocation,
                    environment,
                    fullLimit,
                    failFreeLimit,
                    invocationComplexity.toInt,
                    paymentsComplexity.toInt
                  )
                } yield (environment.currentDiff, result, log)
              })
              TracedResult(
                scriptResultE,
                List(
                  InvokeScriptTrace(
                    tx.id.value(),
                    tx.dAppAddressOrAlias,
                    functionCall,
                    scriptResultE.map(_._2),
                    scriptResultE.fold(_.log, _._3)
                  )
                )
              )
            }

            otherIssues = invocationDiff.scriptResults.get(tx.id()).fold(Seq.empty[Issue])(allIssues)

            doProcessActions = InvokeDiffsCommon.processActions(
              _,
              version,
              dAppAddress,
              pk,
              fixedInvocationComplexity,
              tx,
              CompositeBlockchain(blockchain, Some(invocationDiff)),
              blockTime,
              _,
              isSyncCall = false,
              limitedExecution,
              otherIssues
            )

            resultDiff <- scriptResult match {
              case ScriptResultV3(dataItems, transfers, unusedComplexity) => doProcessActions(dataItems ::: transfers, unusedComplexity)
              case ScriptResultV4(actions, unusedComplexity, _)           => doProcessActions(actions, unusedComplexity)
              case _: IncompleteResult if limitedExecution                => doProcessActions(Nil, 0)
              case i: IncompleteResult =>
                TracedResult(Left(GenericError(s"Evaluation was uncompleted with unused complexity = ${i.unusedComplexity}")))
            }
          } yield invocationDiff |+| resultDiff
        } yield result

      case Left(l) => TracedResult(Left(l))
      case _       => TracedResult(Left(GenericError(s"No contract at address ${tx.dAppAddressOrAlias}")))
    }
  }

  private def evaluateV2(
      version: StdLibVersion,
      contract: DApp,
      invocation: ContractEvaluator.Invocation,
      environment: Environment[Id],
      limit: Int,
      failFreeLimit: Int,
      estimatedComplexity: Int,
      paymentsComplexity: Int
  ): Either[ValidationError with WithLog, (ScriptResult, Log[Id])] = {
    val evaluationCtx = CachedDAppCTX.forVersion(version).completeContext(environment)
    val startLimit    = limit - paymentsComplexity
    ContractEvaluator
      .applyV2Coeval(evaluationCtx, Map(), contract, invocation, version, startLimit)
      .runAttempt()
      .leftMap(error => (error.getMessage: ExecutionError, 0, Nil: Log[Id]))
      .flatten
      .leftMap {
        case (error, unusedComplexity, log) =>
          val usedComplexity = startLimit - unusedComplexity.max(0)
          if (usedComplexity > failFreeLimit) {
            val storingComplexity = Math.max(usedComplexity, estimatedComplexity)
            FailedTransactionError.dAppExecution(error, storingComplexity + paymentsComplexity, log)
          } else
            ScriptExecutionError.dAppExecution(error, log)
      }
  }

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

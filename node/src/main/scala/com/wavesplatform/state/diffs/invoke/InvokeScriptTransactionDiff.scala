package com.wavesplatform.state.diffs.invoke

import scala.util.Right

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
import com.wavesplatform.metrics.{TxProcessingStats => Stats}
import com.wavesplatform.metrics.TxProcessingStats.TxTimerExt
import com.wavesplatform.state._
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction.smart.{DApp => DAppTarget, _}
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.{InvokeScriptTrace, TracedResult}
import monix.eval.Coeval
import shapeless.Coproduct

object InvokeScriptTransactionDiff {

  private[this] def allIssues(r: InvokeScriptResult): Seq[Issue] = {
    r.issues ++ r.invokes.flatMap(s => allIssues(s.stateChanges))
  }

  def apply(blockchain: Blockchain, blockTime: Long, limitedExecution: Boolean)(
      tx: InvokeScriptTransaction
  ): TracedResult[ValidationError, Diff] = {

    val dAppAddressEi = blockchain.resolveAlias(tx.dAppAddressOrAlias)
    val accScriptEi =
      for (address <- dAppAddressEi;
           script  <- blockchain.accountScript(address).toRight(GenericError(s"No contract at address ${tx.dAppAddressOrAlias}")))
        yield (address, script)
    val functionCall = tx.funcCall

    def executeInvoke(
        pk: PublicKey,
        version: StdLibVersion,
        contract: DApp,
        dAppAddress: Address,
        invocationComplexity: Int,
        fixedInvocationComplexity: Int,
        environment: DAppEnvironment,
        invocation: ContractEvaluator.Invocation
    ) = {
      case class MainScriptResult(
          invocationDiff: Diff,
          scriptResult: ScriptResult,
          log: Log[Id],
          availableActions: Int,
          availableData: Int,
          limit: Int
      )

      def executeMainScript(): TracedResult[ValidationError, MainScriptResult] = {
        val scriptResultE = Stats.invokedScriptExecution.measureForType(InvokeScriptTransaction.typeId) {
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

          val paymentsComplexity = tx.checkedAssets.flatMap(blockchain.assetScript).map(_.complexity.toInt).sum

          for {
            (result, log) <- evaluateV2(
              version,
              contract,
              invocation,
              environment,
              fullLimit,
              failFreeLimit,
              invocationComplexity,
              paymentsComplexity,
              blockchain
            )
          } yield MainScriptResult(
            environment.currentDiff,
            result,
            log,
            environment.availableActions,
            environment.availableData,
            fullLimit - paymentsComplexity
          )
        }

        TracedResult(
          scriptResultE,
          List(
            InvokeScriptTrace(
              tx.id(),
              tx.dAppAddressOrAlias,
              functionCall,
              scriptResultE.map(_.scriptResult),
              scriptResultE.fold(_.log, _.log)
            )
          )
        )
      }

      for {
        MainScriptResult(invocationDiff, scriptResult, log, availableActions, availableData, limit) <- executeMainScript()
        otherIssues = invocationDiff.scriptResults.get(tx.id()).fold(Seq.empty[Issue])(allIssues)

        doProcessActions = InvokeDiffsCommon.processActions(
          _,
          version,
          dAppAddress,
          pk,
          _,
          tx,
          CompositeBlockchain(blockchain, invocationDiff),
          blockTime,
          isSyncCall = false,
          limitedExecution,
          ContractLimits.MaxTotalInvokeComplexity(version),
          otherIssues
        )

        process = (actions: List[CallableAction], unusedComplexity: Long) => {
          val storingComplexity = if (blockchain.storeEvaluatedComplexity) limit - unusedComplexity else fixedInvocationComplexity
          val dataCount         = actions.count(_.isInstanceOf[DataOp])
          if (dataCount > availableData) {
            TracedResult(Left(FailedTransactionError.dAppExecution("Stored data count limit is exceeded", storingComplexity, log)))
          } else {
            val actionsCount = actions.length - dataCount
            if (actionsCount > availableActions) {
              TracedResult(Left(FailedTransactionError.dAppExecution("Actions count limit is exceeded", storingComplexity, log)))
            } else {
              doProcessActions(actions, storingComplexity.toInt)
            }
          }
        }

        resultDiff <- scriptResult match {
          case ScriptResultV3(dataItems, transfers, unusedComplexity) =>
            process(dataItems ::: transfers, unusedComplexity)
          case ScriptResultV4(actions, unusedComplexity, _) =>
            process(actions, unusedComplexity)
          case _: IncompleteResult if limitedExecution => doProcessActions(Nil, 0)
          case i: IncompleteResult =>
            TracedResult(Left(GenericError(s"Evaluation was uncompleted with unused complexity = ${i.unusedComplexity}")))
        }
      } yield invocationDiff.copy(scriptsComplexity = 0) |+| resultDiff
    }

    def calcInvocationComplexity(
        version: StdLibVersion,
        callableComplexities: Map[Int, Map[String, Long]],
        dAppAddress: Address
    ): TracedResult[ValidationError, (Int, Int)] = {
      for {
        invocationComplexity <- TracedResult {
          InvokeDiffsCommon.getInvocationComplexity(blockchain, tx.funcCall, callableComplexities, dAppAddress)
        }

        stepLimit = ContractLimits.MaxCallableComplexityByVersion(version)

        fixedInvocationComplexity = if (blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls) && callableComplexities.contains(
                                          ScriptEstimatorV2.version
                                        ))
          Math.min(invocationComplexity, stepLimit)
        else
          invocationComplexity

        _ <- InvokeDiffsCommon.calcAndCheckFee(
          (message, _) => GenericError(message),
          tx,
          blockchain,
          stepLimit,
          fixedInvocationComplexity,
          issueList = Nil,
          additionalScriptsInvoked = 0
        )
      } yield (invocationComplexity.toInt, fixedInvocationComplexity.toInt)
    }

    (accScriptEi: @unchecked) match {
      case Right((dAppAddress, AccountScriptInfo(pk, ContractScriptImpl(version, contract), _, callableComplexities))) =>
        val invocationTracker = DAppEnvironment.InvocationTreeTracker(DAppEnvironment.DAppInvocation(dAppAddress, tx.funcCall, tx.payments))
        (for {
          _                                                 <- TracedResult(checkCall(functionCall, blockchain).leftMap(GenericError(_)))
          (invocationComplexity, fixedInvocationComplexity) <- calcInvocationComplexity(version, callableComplexities, dAppAddress)

          (directives, payments, tthis, input) <- TracedResult(for {
            directives <- DirectiveSet(version, Account, DAppType)
            payments   <- AttachedPaymentExtractor.extractPayments(tx, version, blockchain, DAppTarget)
            tthis = Coproduct[Environment.Tthis](Recipient.Address(ByteStr(dAppAddress.bytes)))
            input <- buildThisValue(Coproduct[TxOrd](tx: Transaction), blockchain, directives, tthis)
          } yield (directives, payments, tthis, input)).leftMap(GenericError(_))

          invoker = Recipient.Address(ByteStr(tx.sender.toAddress.bytes))
          invocation = ContractEvaluator.Invocation(
            functionCall,
            invoker,
            tx.sender,
            invoker,
            tx.sender,
            payments,
            tx.id(),
            tx.fee,
            tx.feeAssetId.compatId
          )

          environment = new DAppEnvironment(
            AddressScheme.current.chainId,
            Coeval.evalOnce(input),
            Coeval.evalOnce(blockchain.height),
            blockchain,
            tthis,
            directives,
            Some(tx),
            dAppAddress,
            pk,
            Set(tx.senderAddress),
            limitedExecution,
            ContractLimits.MaxTotalInvokeComplexity(version),
            ContractLimits.MaxSyncDAppCalls(version),
            ContractLimits.MaxCallableActionsAmount(version),
            ContractLimits.MaxWriteSetSize(version),
            if (version < V5) Diff.empty else InvokeDiffsCommon.paymentsPart(tx, dAppAddress, Map()),
            invocationTracker
          )

          result <- executeInvoke(pk, version, contract, dAppAddress, invocationComplexity, fixedInvocationComplexity, environment, invocation)
        } yield result).leftMap {
          case fte: FailedTransactionError => fte.copy(invocations = invocationTracker.toInvocationList)
          case other                       => other
        }

      case Left(error) => TracedResult(Left(error))
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
      paymentsComplexity: Int,
      blockchain: Blockchain
  ): Either[ValidationError with WithLog, (ScriptResult, Log[Id])] = {
    val evaluationCtx = CachedDAppCTX.get(version, blockchain).completeContext(environment)
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
            val storingComplexity = if (blockchain.storeEvaluatedComplexity) usedComplexity else estimatedComplexity
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

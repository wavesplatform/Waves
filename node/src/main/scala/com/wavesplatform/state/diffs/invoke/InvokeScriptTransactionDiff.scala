package com.wavesplatform.state.diffs.invoke

import cats.Id
import cats.implicits.catsSyntaxSemigroup
import cats.syntax.either.*
import cats.syntax.flatMap.*
import com.wavesplatform.account.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.RideV6
import com.wavesplatform.features.EstimatorProvider.*
import com.wavesplatform.features.EvaluatorFixProvider.*
import com.wavesplatform.features.FunctionCallPolicyProvider.*
import com.wavesplatform.lang.*
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{DApp as DAppType, *}
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.ContractCompiler
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.evaluator.*
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.{Recipient as RideRecipient, *}
import com.wavesplatform.metrics.TxProcessingStats as Stats
import com.wavesplatform.metrics.TxProcessingStats.TxTimerExt
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.invoke.CallArgumentPolicy.*
import com.wavesplatform.state.reader.SnapshotBlockchain
import com.wavesplatform.transaction.TransactionBase
import com.wavesplatform.transaction.TxValidationError.*
import com.wavesplatform.transaction.smart.DAppEnvironment.ActionLimits
import com.wavesplatform.transaction.smart.InvokeTransaction.DefaultCall
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.{InvokeScriptTrace, TracedResult}
import com.wavesplatform.transaction.smart.{DApp as DAppTarget, *}
import com.wavesplatform.transaction.validation.impl.DataTxValidator
import monix.eval.Coeval
import shapeless.Coproduct

object InvokeScriptTransactionDiff {

  private[this] def allIssues(r: InvokeScriptResult): Seq[Issue] = {
    r.issues ++ r.invokes.flatMap(s => allIssues(s.stateChanges))
  }

  def apply(blockchain: Blockchain, blockTime: Long, limitedExecution: Boolean, enableExecutionLog: Boolean)(
      tx: InvokeScriptTransactionLike
  ): TracedResult[ValidationError, StateSnapshot] = {

    val accScriptEi =
      for {
        address <- blockchain.resolveAlias(tx.dApp)
        scriptOpt = blockchain.accountScript(address)
        script <- tx match {
          case ie: InvokeExpressionTransaction => extractFreeCall(ie, blockchain)
          case _                               => extractInvoke(tx, scriptOpt)
        }
      } yield (address, script)

    def executeInvoke(
        pk: PublicKey,
        version: StdLibVersion,
        contract: DApp,
        dAppAddress: Address,
        environment: DAppEnvironment,
        invocation: ContractEvaluator.Invocation
    ) = {
      case class MainScriptResult(
          invocationSnapshot: StateSnapshot,
          scriptResult: ScriptResult,
          log: Log[Id],
          availableActions: ActionLimits,
          limit: Int
      )

      def executeMainScript(): TracedResult[ValidationError, MainScriptResult] = {
        val scriptResultE = Stats.invokedScriptExecution.measureForType(tx.tpe) {
          val fullLimit =
            if (blockchain.estimator == ScriptEstimatorV2)
              Int.MaxValue // to avoid continuations when evaluating underestimated by EstimatorV2 scripts
            else if (limitedExecution)
              ContractLimits.FailFreeInvokeComplexity
            else
              ContractLimits.MaxTotalInvokeComplexity(version)

          val failFreeLimit =
            if (blockchain.isFeatureActivated(BlockchainFeatures.BlockV5))
              ContractLimits.FailFreeInvokeComplexity
            else
              fullLimit

          val paymentsComplexity = tx.smartAssets(blockchain).flatMap(blockchain.assetScript).map(_.complexity.toInt).sum

          for {
            (result, log) <- evaluateV2(
              version,
              contract,
              dAppAddress,
              invocation,
              environment,
              fullLimit,
              failFreeLimit,
              paymentsComplexity,
              blockchain,
              enableExecutionLog
            )
          } yield MainScriptResult(
            environment.currentSnapshot,
            result,
            log,
            environment.availableActions,
            fullLimit - paymentsComplexity
          )
        }

        TracedResult(
          scriptResultE,
          List(
            InvokeScriptTrace(
              tx.id(),
              tx.dApp,
              tx.funcCall,
              scriptResultE.map(_.scriptResult),
              scriptResultE.fold(
                {
                  case w: WithLog => w.log
                  case _          => Nil
                },
                _.log
              ),
              environment.invocationRoot.toTraceList(tx.id())
            )
          )
        )
      }

      for {
        MainScriptResult(
          invocationSnapshot,
          scriptResult,
          log,
          availableActions,
          limit
        ) <- executeMainScript()
        otherIssues = invocationSnapshot.scriptResults.get(tx.id()).fold(Seq.empty[Issue])(allIssues)

        doProcessActions = InvokeDiffsCommon.processActions(
          _,
          version,
          version,
          dAppAddress,
          pk,
          _,
          tx,
          SnapshotBlockchain(blockchain, invocationSnapshot),
          blockTime,
          isSyncCall = false,
          limitedExecution,
          ContractLimits.MaxTotalInvokeComplexity(version),
          otherIssues,
          enableExecutionLog,
          log
        )

        process = (actions: List[CallableAction], unusedComplexity: Long) => {
          val storingComplexity = limit - unusedComplexity

          val dataEntries  = actions.collect { case d: DataOp => InvokeDiffsCommon.dataItemToEntry(d) }
          val dataCount    = dataEntries.length
          val dataSize     = DataTxValidator.invokeWriteSetSize(blockchain, dataEntries)
          val actionsCount = actions.length - dataCount
          val balanceActionsCount = actions.collect {
            case tr: AssetTransfer => tr
            case l: Lease          => l
            case lc: LeaseCancel   => lc
          }.length
          val assetActionsCount = actions.length - dataCount - balanceActionsCount

          for {
            _ <- InvokeDiffsCommon.checkCallResultLimits(
              version,
              version,
              blockchain,
              storingComplexity,
              log,
              actionsCount,
              balanceActionsCount,
              assetActionsCount,
              dataCount,
              dataSize,
              availableActions
            )
            diff <- doProcessActions(StructuredCallableActions(actions, blockchain), storingComplexity.toInt)
          } yield diff
        }

        resultDiff <- scriptResult match {
          case ScriptResultV3(dataItems, transfers, unusedComplexity) =>
            process(dataItems ::: transfers, unusedComplexity)
          case ScriptResultV4(actions, unusedComplexity, _) =>
            process(actions, unusedComplexity)
          case _: IncompleteResult if limitedExecution =>
            doProcessActions(StructuredCallableActions(Nil, blockchain), 0)
          case i: IncompleteResult =>
            TracedResult(Left(GenericError(s"Evaluation was uncompleted with unused complexity = ${i.unusedComplexity}")))
        }
        totalDiff = invocationSnapshot.setScriptsComplexity(0) |+| resultDiff
      } yield totalDiff
    }

    accScriptEi match {
      case Right((dAppAddress, (pk, version, funcCall, contract, _))) =>
        val invocationTracker = DAppEnvironment.InvocationTreeTracker(DAppEnvironment.DAppInvocation(dAppAddress, funcCall, tx.payments))
        (for {
          _ <- TracedResult(checkCall(funcCall, blockchain).leftMap(GenericError(_)))
          (directives, tthis, input) <- TracedResult(for {
            directives <- DirectiveSet(version, Account, DAppType)
            tthis = Coproduct[Environment.Tthis](RideRecipient.Address(ByteStr(dAppAddress.bytes)))
            input <- buildThisValue(Coproduct[TxOrd](tx: TransactionBase), blockchain, directives, tthis)
          } yield (directives, tthis, input)).leftMap(GenericError(_))

          paymentsPart <- TracedResult(
            if (version < V5) Right(StateSnapshot.empty)
            else InvokeDiffsCommon.paymentsPart(blockchain, tx, dAppAddress, Map())
          )

          environment = new DAppEnvironment(
            AddressScheme.current.chainId,
            Coeval.evalOnce(input),
            Coeval.evalOnce(blockchain.height),
            blockchain,
            tthis,
            directives,
            version,
            tx,
            dAppAddress,
            pk,
            Set(tx.sender.toAddress),
            limitedExecution,
            enableExecutionLog,
            ContractLimits.MaxTotalInvokeComplexity(version),
            ContractLimits.MaxSyncDAppCalls(version),
            ActionLimits(
              ContractLimits.MaxCallableActionsAmountBeforeV6(version),
              ContractLimits.MaxBalanceScriptActionsAmountV6,
              ContractLimits.MaxAssetScriptActionsAmountV6,
              ContractLimits.MaxWriteSetSize,
              ContractLimits.MaxTotalWriteSetSizeInBytes
            ),
            ContractLimits.MaxTotalPaymentAmountRideV6 - tx.payments.size,
            paymentsPart,
            invocationTracker
          )
          invoker  = RideRecipient.Address(ByteStr(tx.sender.toAddress.bytes))
          payments = AttachedPaymentExtractor.extractPayments(tx, version, blockchain, DAppTarget).explicitGet()
          invocation = ContractEvaluator.Invocation(
            funcCall,
            invoker,
            tx.sender,
            invoker,
            tx.sender,
            payments,
            tx.id(),
            tx.fee,
            tx.feeAssetId.compatId
          )
          result <- executeInvoke(pk, version, contract, dAppAddress, environment, invocation)
        } yield result).leftMap {
          case fte: FailedTransactionError => fte.copy(invocations = invocationTracker.toInvocationList)
          case other                       => other
        }

      case Left(error) => TracedResult(Left(error))
    }
  }

  private def extractInvoke(
      tx: InvokeScriptTransactionLike,
      scriptOpt: Option[AccountScriptInfo]
  ): Either[GenericError, (PublicKey, StdLibVersion, FUNCTION_CALL, DApp, Map[Int, Map[String, Long]])] =
    scriptOpt
      .map {
        case AccountScriptInfo(publicKey, ContractScriptImpl(version, dApp), _, complexities) =>
          Right((publicKey, version, tx.funcCall, dApp, complexities))
        case _ =>
          InvokeDiffsCommon.callExpressionError
      }
      .getOrElse(Left(GenericError(s"No contract at address ${tx.dApp}")))

  private def extractFreeCall(
      tx: InvokeExpressionTransaction,
      blockchain: Blockchain
  ): Either[GenericError, (PublicKey, StdLibVersion, FUNCTION_CALL, DApp, Map[Int, Map[String, Long]])] = {
    val annotation = CallableAnnotation(ContractCompiler.FreeCallInvocationArg)
    val callable   = CallableFunction(annotation, FUNC(DefaultCall.function.funcName, Nil, tx.expression.expr))
    val dApp       = DApp(DAppMeta(), Nil, List(callable), None)
    val version    = tx.expression.stdLibVersion
    val estimator  = blockchain.estimator
    ContractScript
      .estimateComplexity(version, dApp, estimator, fixEstimateOfVerifier = blockchain.isFeatureActivated(RideV6))
      .leftMap(GenericError(_))
      .map { case (_, complexities) =>
        (tx.sender, version, DefaultCall, dApp, Map(estimator.version -> complexities))
      }
  }

  private def evaluateV2(
      version: StdLibVersion,
      contract: DApp,
      dAppAddress: Address,
      invocation: ContractEvaluator.Invocation,
      environment: Environment[Id],
      limit: Int,
      failFreeLimit: Int,
      paymentsComplexity: Int,
      blockchain: Blockchain,
      enableExecutionLog: Boolean
  ): Either[ValidationError, (ScriptResult, Log[Id])] = {
    val evaluationCtx = CachedDAppCTX.get(version, blockchain).completeContext(environment)
    val startLimit    = limit - paymentsComplexity
    ContractEvaluator
      .applyV2Coeval(
        evaluationCtx,
        contract,
        ByteStr(dAppAddress.bytes),
        invocation,
        version,
        startLimit,
        blockchain.correctFunctionCallScope,
        blockchain.newEvaluatorMode,
        enableExecutionLog
      )
      .runAttempt()
      .leftMap(error => (error.getMessage: ExecutionError, 0, Nil: Log[Id]))
      .flatten
      .leftMap[ValidationError] {
        case (FailOrRejectError(msg, true), _, log) =>
          InvokeRejectError(msg, log)
        case (error, unusedComplexity, log) =>
          val usedComplexity = startLimit - unusedComplexity.max(0)
          val msg = error match {
            case CommonError(_, Some(fte: FailedTransactionError)) => fte.error.getOrElse(error.message)
            case _                                                 => error.message
          }
          if (usedComplexity > failFreeLimit) {
            FailedTransactionError.dAppExecution(msg, usedComplexity + paymentsComplexity, log)
          } else
            InvokeRejectError(msg, log)
      }
      .flatTap { case (r, log) =>
        InvokeDiffsCommon
          .checkScriptResultFields(blockchain, r)
          .leftMap[ValidationError] {
            case FailOrRejectError(message, true) =>
              InvokeRejectError(message, log)
            case error =>
              val usedComplexity = startLimit - r.unusedComplexity
              val msg = error match {
                case fte: FailedTransactionError => fte.error.getOrElse(error.toString)
                case _                           => error.toString
              }
              if (usedComplexity > failFreeLimit) {
                FailedTransactionError.dAppExecution(msg, usedComplexity + paymentsComplexity, log)
              } else
                InvokeRejectError(msg, log)
          }
      }
  }

  private def checkCall(fc: FUNCTION_CALL, blockchain: Blockchain): Either[String, Unit] = {
    val policy =
      if (blockchain.callableListArgumentsCorrected)
        CallArgumentPolicy.PrimitivesAndListsOfPrimitives
      else if (blockchain.callableListArgumentsAllowed)
        CallArgumentPolicy.PrimitivesAndLists
      else
        CallArgumentPolicy.OnlyPrimitives
    fc.check(policy)
  }
}

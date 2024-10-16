package com.wavesplatform.state.diffs.invoke

import cats.Id
import cats.implicits.{catsSyntaxSemigroup, toFlatMapOps}
import cats.instances.list.*
import cats.syntax.either.*
import cats.syntax.traverseFilter.*
import com.wavesplatform.account.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.LightNode
import com.wavesplatform.features.EstimatorProvider.EstimatorBlockchainExt
import com.wavesplatform.features.EvaluatorFixProvider.*
import com.wavesplatform.features.FunctionCallPolicyProvider.*
import com.wavesplatform.lang.*
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{DApp as DAppType, *}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.unit
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, IncompleteResult, Log, ScriptResult, ScriptResultV3, ScriptResultV4}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.Tx.{InvokePseudoTx, ScriptTransfer}
import com.wavesplatform.lang.v1.traits.domain.{Recipient as RideRecipient, *}
import com.wavesplatform.metrics.*
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.BalanceDiffValidation
import com.wavesplatform.state.diffs.invoke.CallArgumentPolicy.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.*
import com.wavesplatform.transaction.smart.DAppEnvironment.ActionLimits
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.CoevalR.traced
import com.wavesplatform.transaction.smart.script.trace.{AssetVerifierTrace, CoevalR, TracedResult}
import com.wavesplatform.transaction.smart.{DApp as DAppTarget, *}
import com.wavesplatform.transaction.validation.impl.DataTxValidator
import com.wavesplatform.transaction.{TransactionType, TxValidationError}
import monix.eval.Coeval

object InvokeScriptDiff {
  private val stats = TxProcessingStats
  import stats.TxTimerExt

  def apply(
      blockchain: Blockchain,
      blockTime: Long,
      rootVersion: StdLibVersion,
      limitedExecution: Boolean,
      enableExecutionLog: Boolean,
      totalComplexityLimit: Int,
      remainingComplexity: Int,
      remainingCalls: Int,
      remainingActions: ActionLimits,
      remainingPayments: Int,
      calledAddresses: Set[Address],
      invocationRoot: DAppEnvironment.InvocationTreeTracker,
      wrapDAppEnv: DAppEnvironment => DAppEnvironmentInterface = identity
  )(
      tx: InvokeScript
  ): CoevalR[(StateSnapshot, EVALUATED, ActionLimits, Int)] = {
    val dAppAddress = tx.dApp
    val invoker     = tx.sender.toAddress

    val result = blockchain.accountScript(dAppAddress) match {
      case Some(AccountScriptInfo(pk, ContractScriptImpl(version, contract), _, _)) =>
        for {
          _ <- traced {
            if (blockchain.checkSyncCallArgumentsTypes)
              tx.funcCall.check(CallArgumentPolicy.PrimitivesAndListsOfPrimitives).leftMap(GenericError(_))
            else
              Right(())
          }
          _ <- traced(
            Either.cond(
              version >= V5,
              (),
              GenericError(
                s"DApp $invoker invoked DApp $dAppAddress that uses RIDE $version, but dApp-to-dApp invocation requires version 5 or higher"
              )
            )
          )
          _ <- traced(
            Either.cond(
              remainingCalls > 0,
              (),
              ValidationError.ScriptRunsLimitError(s"DApp calls limit = ${ContractLimits.MaxSyncDAppCalls(version)} is exceeded")
            )
          )
          _ <- traced(
            Either.cond(
              !blockchain.isFeatureActivated(BlockchainFeatures.RideV6) || remainingPayments >= tx.payments.size,
              (),
              GenericError(s"Invoke payments limit = ${ContractLimits.MaxTotalPaymentAmountRideV6} is exceeded")
            )
          )
          _ <- ensurePaymentsAreNotNegative(blockchain, tx, invoker, dAppAddress)

          directives: DirectiveSet <- traced(DirectiveSet(version, Account, DAppType).leftMap(GenericError.apply))
          payments <- traced(AttachedPaymentExtractor.extractPayments(tx, version, blockchain, DAppTarget).leftMap(GenericError.apply))
          checkedPayments <- traced {
            payments.payments.toList
              .traverseFilter {
                case (amount, Some(assetId)) =>
                  InvokeDiffsCommon
                    .checkAsset(blockchain, assetId)
                    .map(_ => blockchain.assetScript(IssuedAsset(assetId)).flatMap(s => Some((s, amount, assetId))))
                case _ => Right(None)
              }
              .leftMap(GenericError(_))
          }
          complexityAfterPaymentsTraced = checkedPayments.foldLeft(TracedResult(Right(remainingComplexity): TxValidationError.Validation[Int])) {
            case (error @ TracedResult(Left(_), _, _), _) => error
            case (TracedResult(Right(nextRemainingComplexity), _, _), (script, amount, assetId)) =>
              val usedComplexity = totalComplexityLimit - nextRemainingComplexity
              val pseudoTx = if (blockchain.isFeatureActivated(BlockchainFeatures.RideV6)) {
                InvokePseudoTx(
                  tx.txId,
                  tx.timestamp,
                  RideRecipient.Address(ByteStr(tx.sender.toAddress.bytes)),
                  tx.sender,
                  RideRecipient.Address(ByteStr(tx.dApp.bytes)),
                  None,
                  Some(tx.funcCall.function.funcName),
                  tx.funcCall.args.collect { case ev: EVALUATED => ev },
                  payments
                )
              } else {
                ScriptTransfer(
                  Some(assetId),
                  RideRecipient.Address(ByteStr(tx.sender.toAddress.bytes)),
                  tx.sender,
                  RideRecipient.Address(ByteStr(tx.dApp.bytes)),
                  amount,
                  tx.timestamp,
                  tx.txId
                )
              }
              val (log, evaluatedComplexity, result) = ScriptRunner(
                pseudoTx: PseudoTx,
                blockchain,
                script.script,
                isAssetScript = true,
                scriptContainerAddress = Environment.AssetId(assetId.arr),
                enableExecutionLog = enableExecutionLog,
                nextRemainingComplexity
              )
              val scriptComplexity = if (blockchain.storeEvaluatedComplexity) evaluatedComplexity else script.complexity.toInt
              val totalComplexity  = usedComplexity + scriptComplexity
              result match {
                case Left(error) =>
                  val err = FailedTransactionError.assetExecutionInAction(error.message, totalComplexity, log, assetId)
                  TracedResult(Left(err), List(AssetVerifierTrace(assetId, Some(err))))
                case Right(FALSE) =>
                  val err = FailedTransactionError.notAllowedByAsset(totalComplexity, log, assetId)
                  TracedResult(Left(err), List(AssetVerifierTrace(assetId, Some(err))))
                case Right(TRUE) =>
                  TracedResult(Right(nextRemainingComplexity - scriptComplexity))
                case Right(x) =>
                  val err = FailedTransactionError.assetExecution(s"Script returned not a boolean result, but $x", totalComplexity, log, assetId)
                  TracedResult(Left(err), List(AssetVerifierTrace(assetId, Some(err))))
              }
          }
          complexityAfterPayments <- CoevalR(Coeval.now(complexityAfterPaymentsTraced))
          paymentsComplexity = checkedPayments.map(_._1.complexity).sum.toInt

          tthis = RideRecipient.Address(ByteStr(dAppAddress.bytes))
          input <- traced(buildThisValue(tx.root, blockchain, directives, tthis).leftMap(GenericError(_)))

          result <- for {
            paymentsPart <- traced(InvokeDiffsCommon.paymentsPart(blockchain, tx, tx.dApp, Map()))
            (
              resultSnapshot,
              (scriptResult, log),
              availableActions,
              availablePayments
            ) <- {
              stats.invokedScriptExecution.measureForType(TransactionType.InvokeScript)({
                val height = blockchain.height
                val invocation = ContractEvaluator.Invocation(
                  tx.funcCall,
                  RideRecipient.Address(ByteStr(invoker.bytes)),
                  ByteStr(tx.sender.arr),
                  RideRecipient.Address(ByteStr(tx.root.sender.toAddress.bytes)),
                  ByteStr(tx.root.sender.arr),
                  payments,
                  tx.txId,
                  tx.root.fee,
                  tx.root.feeAssetId.compatId
                )
                val (paymentsPartInsideDApp, paymentsPartToResolve) =
                  if (version < V5) (StateSnapshot.empty, paymentsPart) else (paymentsPart, StateSnapshot.empty)
                val environment = wrapDAppEnv(new DAppEnvironment(
                  AddressScheme.current.chainId,
                  Coeval.evalOnce(input),
                  Coeval(height),
                  blockchain,
                  tthis,
                  directives,
                  rootVersion,
                  tx.root,
                  tx.dApp,
                  pk,
                  calledAddresses,
                  limitedExecution,
                  enableExecutionLog,
                  totalComplexityLimit,
                  remainingCalls - 1,
                  remainingActions,
                  remainingPayments - tx.payments.size,
                  paymentsPartInsideDApp,
                  invocationRoot,
                  wrapDAppEnv
                ))
                for {
                  _ <-
                    if (
                      blockchain.height >= blockchain.settings.functionalitySettings.paymentsCheckHeight && blockchain
                        .isFeatureActivated(BlockchainFeatures.LightNode)
                    )
                      validateIntermediateBalances(blockchain, paymentsPartInsideDApp, 0, Nil)
                    else traced(Right(()))
                  evaluated <- CoevalR(
                    evaluateV2(
                      version,
                      blockchain,
                      contract,
                      dAppAddress,
                      invocation,
                      environment,
                      complexityAfterPayments,
                      remainingComplexity,
                      enableExecutionLog
                    ).map(TracedResult(_))
                  )
                  resultSnapshot = environment.currentSnapshot |+| paymentsPartToResolve
                } yield (
                  resultSnapshot,
                  evaluated,
                  environment.availableActions,
                  environment.availablePayments
                )
              })
            }
            _               = invocationRoot.setLog(log)
            spentComplexity = remainingComplexity - scriptResult.unusedComplexity.max(0)

            _ <-
              if (blockchain.isFeatureActivated(LightNode))
                traced(Right(()))
              else
                validateIntermediateBalances(blockchain, resultSnapshot, spentComplexity, log)

            doProcessActions = (actions: List[CallableAction], unusedComplexity: Int) => {
              val storingComplexity = complexityAfterPayments - unusedComplexity
              CoevalR(
                Coeval.now(
                  InvokeDiffsCommon.processActions(
                    StructuredCallableActions(actions, blockchain),
                    version,
                    rootVersion,
                    dAppAddress,
                    pk,
                    storingComplexity,
                    tx,
                    SnapshotBlockchain(blockchain, resultSnapshot),
                    blockTime,
                    isSyncCall = true,
                    limitedExecution,
                    totalComplexityLimit,
                    Seq(),
                    enableExecutionLog,
                    log
                  )
                )
              )
            }

            process = {
              (
                  actions: List[CallableAction],
                  unusedComplexity: Int,
                  actionsCount: Int,
                  balanceActionsCount: Int,
                  assetActionsCount: Int,
                  dataCount: Int,
                  dataSize: Int,
                  ret: EVALUATED
              ) =>
                for {
                  _ <- CoevalR(
                    Coeval(
                      InvokeDiffsCommon.checkCallResultLimits(
                        version,
                        rootVersion,
                        blockchain,
                        remainingComplexity - unusedComplexity,
                        log,
                        actionsCount,
                        balanceActionsCount,
                        assetActionsCount,
                        dataCount,
                        dataSize,
                        availableActions
                      )
                    )
                  )
                  snapshot <- doProcessActions(actions, unusedComplexity)
                } yield (
                  snapshot,
                  ret,
                  availableActions.decrease(actionsCount, balanceActionsCount, assetActionsCount, dataCount, dataSize),
                  availablePayments
                )
            }

            (
              actionsSnapshot,
              evaluated,
              remainingActions1,
              remainingPayments1
            ) <-
              scriptResult match {
                case ScriptResultV3(dataItems, transfers, unusedComplexity) =>
                  val dataEntries  = dataItems.map(InvokeDiffsCommon.dataItemToEntry)
                  val dataCount    = dataItems.length
                  val dataSize     = DataTxValidator.invokeWriteSetSize(blockchain, dataEntries)
                  val actionsCount = transfers.length
                  process(dataItems ::: transfers, unusedComplexity, actionsCount, actionsCount, 0, dataCount, dataSize, unit)
                case ScriptResultV4(actions, unusedComplexity, ret) =>
                  val dataEntries = actions.collect { case d: DataOp => InvokeDiffsCommon.dataItemToEntry(d) }
                  val dataCount   = dataEntries.length
                  val balanceActionsCount = actions.collect {
                    case tr: AssetTransfer => tr
                    case l: Lease          => l
                    case lc: LeaseCancel   => lc
                  }.length
                  val assetActionsCount = actions.length - dataCount - balanceActionsCount
                  val dataSize          = DataTxValidator.invokeWriteSetSize(blockchain, dataEntries)
                  val actionsCount      = actions.length - dataCount
                  process(actions, unusedComplexity, actionsCount, balanceActionsCount, assetActionsCount, dataCount, dataSize, ret)
                case _: IncompleteResult if limitedExecution =>
                  doProcessActions(Nil, 0).map(
                    (
                      _,
                      unit,
                      availableActions,
                      availablePayments
                    )
                  )
                case r: IncompleteResult =>
                  val usedComplexity = remainingComplexity - r.unusedComplexity
                  val error =
                    FailedTransactionError.dAppExecution(s"Invoke complexity limit = $totalComplexityLimit is exceeded", usedComplexity, log)
                  traced(error.asLeft[(StateSnapshot, EVALUATED, ActionLimits, Int)])
              }
            resultSnapshot <- traced(
              (resultSnapshot.setScriptsComplexity(0) |+| actionsSnapshot.addScriptsComplexity(paymentsComplexity)).asRight
            )
            _ <-
              if (blockchain.isFeatureActivated(LightNode))
                traced(Right(()))
              else
                validateIntermediateBalances(blockchain, resultSnapshot, resultSnapshot.scriptsComplexity, log)
            _ = invocationRoot.setResult(scriptResult)
          } yield (
            resultSnapshot,
            evaluated,
            remainingActions1,
            remainingPayments1
          )
        } yield result

      case Some(AccountScriptInfo(_, _, _, _)) => traced(InvokeDiffsCommon.callExpressionError)
      case _                                   => traced(Left(GenericError(s"No contract at address ${tx.dApp}")))
    }

    result.leftMap { err =>
      invocationRoot.setError(err)
      err
    }
  }

  private[invoke] def evaluateV2(
      version: StdLibVersion,
      blockchain: Blockchain,
      contract: DApp,
      dAppAddress: Address,
      invocation: ContractEvaluator.Invocation,
      environment: Environment[Id],
      limit: Int,
      startComplexityLimit: Int,
      enableExecutionLog: Boolean
  ): Coeval[Either[ValidationError, (ScriptResult, Log[Id])]] = {
    val evaluationCtx = CachedDAppCTX.get(version, blockchain).completeContext(environment)
    ContractEvaluator
      .applyV2Coeval(
        evaluationCtx,
        contract,
        ByteStr(dAppAddress.bytes),
        invocation,
        version,
        limit,
        blockchain.correctFunctionCallScope,
        blockchain.newEvaluatorMode,
        enableExecutionLog,
        blockchain.isFeatureActivated(LightNode)
      )
      .map(
        _.leftMap[ValidationError] {
          case (reject @ FailOrRejectError(_, true), _, _) =>
            reject.copy(skipInvokeComplexity = false)
          case (error, unusedComplexity, log) =>
            val usedComplexity = startComplexityLimit - unusedComplexity
            val msg = error match {
              case CommonError(_, Some(fte: FailedTransactionError)) => fte.error.getOrElse(error.message)
              case _                                                 => error.message
            }
            FailedTransactionError.dAppExecution(msg, usedComplexity, log)
        }.flatTap { case (r, log) =>
          InvokeDiffsCommon
            .checkScriptResultFields(blockchain, r)
            .leftMap[ValidationError]({
              case reject: FailOrRejectError => reject
              case error =>
                val usedComplexity = startComplexityLimit - r.unusedComplexity
                val msg = error match {
                  case fte: FailedTransactionError => fte.error.getOrElse(error.toString)
                  case _                           => error.toString
                }
                FailedTransactionError.dAppExecution(msg, usedComplexity, log)
            })
        }
      )
  }

  def validateIntermediateBalances(blockchain: Blockchain, snapshot: StateSnapshot, spentComplexity: Long, log: Log[Id]): CoevalR[Any] =
    traced(
      if (blockchain.isFeatureActivated(BlockchainFeatures.RideV6)) {
        BalanceDiffValidation(blockchain)(snapshot)
          .leftMap { be => FailedTransactionError.dAppExecution(be.toString, spentComplexity, log) }
      } else if (blockchain.height >= blockchain.settings.functionalitySettings.enforceTransferValidationAfter) {
        // reject transaction if any balance is negative
        snapshot.balances.view
          .flatMap {
            case ((address, asset), balance) if balance < 0 => Some(address -> asset)
            case _                                          => None
          }
          .headOption
          .fold[Either[ValidationError, Unit]](Right(())) { case (address, asset) =>
            val msg = asset match {
              case Waves =>
                s"$address: Negative waves balance: old = ${blockchain.balance(address)}, new = ${snapshot.balances((address, Waves))}"
              case ia: IssuedAsset =>
                s"$address: Negative asset $ia balance: old = ${blockchain.balance(address, ia)}, new = ${snapshot.balances((address, ia))}"
            }
            Left(FailOrRejectError(msg))
          }

      } else Right(())
    )

  private def ensurePaymentsAreNotNegative(blockchain: Blockchain, tx: InvokeScript, invoker: Address, dAppAddress: Address) = traced {
    tx.payments.collectFirst {
      case p if p.amount < 0 =>
        s"DApp $invoker invoked DApp $dAppAddress with attached ${p.assetId.fold("WAVES")(a => s"token $a")} amount = ${p.amount}"
    } match {
      case Some(e) if blockchain.isFeatureActivated(BlockchainFeatures.RideV6) =>
        Left(GenericError(e))
      case Some(e)
          if blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls) &&
            blockchain.height >= blockchain.settings.functionalitySettings.enforceTransferValidationAfter =>
        Left(FailOrRejectError(e))
      case _ => Right(())
    }
  }
}

package com.wavesplatform.state.diffs.invoke

import cats.Id
import cats.implicits.toFlatMapOps
import cats.instances.list.*
import cats.syntax.either.*
import cats.syntax.traverseFilter.*
import com.wavesplatform.account.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
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
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.*
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.CoevalR.traced
import com.wavesplatform.transaction.smart.script.trace.{AssetVerifierTrace, CoevalR, TracedResult}
import com.wavesplatform.transaction.smart.{DApp as DAppTarget, *}
import com.wavesplatform.transaction.validation.impl.DataTxValidator
import com.wavesplatform.transaction.{TransactionType, TxValidationError}
import monix.eval.Coeval
import shapeless.Coproduct

object InvokeScriptDiff {
  private val stats = TxProcessingStats
  import stats.TxTimerExt

  def apply(
      blockchain: Blockchain,
      blockTime: Long,
      limitedExecution: Boolean,
      totalComplexityLimit: Int,
      remainingComplexity: Int,
      remainingCalls: Int,
      remainingActions: Int,
      remainingBalanceActionsV6: Int,
      remainingAssetActionsV6: Int,
      remainingPayments: Int,
      remainingData: Int,
      remainingDataSize: Int,
      calledAddresses: Set[Address],
      invocationRoot: DAppEnvironment.InvocationTreeTracker
  )(
      tx: InvokeScript
  ): CoevalR[(Diff, EVALUATED, Int, Int, Int, Int, Int, Int)] = {
    val dAppAddress = tx.dApp
    val invoker     = tx.sender.toAddress

    val result = blockchain.accountScript(dAppAddress) match {
      case Some(AccountScriptInfo(pk, ContractScriptImpl(version, contract), _, callableComplexities)) =>
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
          invocationComplexity <- traced {
            InvokeDiffsCommon.getInvocationComplexity(blockchain, tx.funcCall, callableComplexities, dAppAddress)
          }

          _ <- traced(
            Either.cond(
              invocationComplexity <= ContractLimits.MaxCallableComplexityByVersion(version),
              (),
              GenericError("Continuation is not allowed for Invoke by script")
            )
          )

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
                Coproduct[TxOrd](pseudoTx: PseudoTx),
                blockchain,
                script.script,
                isAssetScript = true,
                scriptContainerAddress = Coproduct[Environment.Tthis](Environment.AssetId(assetId.arr)),
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

          tthis = Coproduct[Environment.Tthis](RideRecipient.Address(ByteStr(dAppAddress.bytes)))
          input <- traced(buildThisValue(Coproduct[TxOrd](tx.root), blockchain, directives, tthis).leftMap(GenericError(_)))

          result <- for {
            paymentsPart <- traced(InvokeDiffsCommon.paymentsPart(tx, tx.dApp, Map()))
            (
              diff,
              (scriptResult, log),
              availableActions,
              availableBalanceActions,
              availableAssetActions,
              availablePayments,
              availableData,
              availableDataSize
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
                val (paymentsPartInsideDApp, paymentsPartToResolve) = if (version < V5) (Diff.empty, paymentsPart) else (paymentsPart, Diff.empty)
                val environment = new DAppEnvironment(
                  AddressScheme.current.chainId,
                  Coeval.evalOnce(input),
                  Coeval(height),
                  blockchain,
                  tthis,
                  directives,
                  tx.root,
                  tx.dApp,
                  pk,
                  calledAddresses,
                  limitedExecution,
                  totalComplexityLimit,
                  remainingCalls - 1,
                  remainingActions,
                  remainingBalanceActionsV6,
                  remainingAssetActionsV6,
                  remainingPayments - tx.payments.size,
                  remainingData,
                  remainingDataSize,
                  paymentsPartInsideDApp,
                  invocationRoot
                )
                for {
                  evaluated <- CoevalR(
                    evaluateV2(
                      version,
                      blockchain,
                      contract,
                      invocation,
                      environment,
                      complexityAfterPayments,
                      remainingComplexity
                    ).map(TracedResult(_))
                  )
                  diff <- traced(environment.currentDiff.combineF(paymentsPartToResolve).leftMap(GenericError(_)))
                } yield (
                  diff,
                  evaluated,
                  environment.availableActions,
                  environment.availableBalanceActions,
                  environment.availableAssetActions,
                  environment.availablePayments,
                  environment.availableData,
                  environment.availableDataSize
                )
              })
            }
            _               = invocationRoot.setLog(log)
            spentComplexity = remainingComplexity - scriptResult.unusedComplexity.max(0)

            _ <- validateIntermediateBalances(blockchain, diff, spentComplexity, log)

            doProcessActions = (actions: List[CallableAction], unusedComplexity: Int) => {
              val storingComplexity = if (blockchain.storeEvaluatedComplexity) complexityAfterPayments - unusedComplexity else invocationComplexity
              CoevalR(
                Coeval.now(
                  InvokeDiffsCommon.processActions(
                    actions,
                    version,
                    dAppAddress,
                    pk,
                    storingComplexity.toInt,
                    tx,
                    CompositeBlockchain(blockchain, diff),
                    blockTime,
                    isSyncCall = true,
                    limitedExecution,
                    totalComplexityLimit,
                    Seq()
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
                        blockchain,
                        remainingComplexity - unusedComplexity,
                        log,
                        actionsCount,
                        balanceActionsCount,
                        assetActionsCount,
                        dataCount,
                        dataSize,
                        availableActions,
                        availableBalanceActions,
                        availableAssetActions,
                        availableData,
                        availableDataSize
                      )
                    )
                  )
                  diff <- doProcessActions(actions, unusedComplexity)
                } yield (
                  diff,
                  ret,
                  availableActions - actionsCount,
                  availableBalanceActions - balanceActionsCount,
                  availableAssetActions - assetActionsCount,
                  availablePayments,
                  availableData - dataCount,
                  availableDataSize - dataSize
                )
            }

            (
              actionsDiff,
              evaluated,
              remainingActions1,
              remainingBalanceActions1,
              remainingAssetActions1,
              remainingPayments1,
              remainingData1,
              remainingDataSize1
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
                    (_, unit, availableActions, availableBalanceActions, availableAssetActions, availablePayments, availableData, availableDataSize)
                  )
                case r: IncompleteResult =>
                  val usedComplexity = remainingComplexity - r.unusedComplexity
                  val error =
                    FailedTransactionError.dAppExecution(s"Invoke complexity limit = $totalComplexityLimit is exceeded", usedComplexity, log)
                  traced(error.asLeft[(Diff, EVALUATED, Int, Int, Int, Int, Int, Int)])
              }
            resultDiff <- traced(
              diff
                .copy(scriptsComplexity = 0)
                .combineE(actionsDiff)
                .flatMap(_.combineE(Diff(scriptsComplexity = paymentsComplexity)))
            )

            _ <- validateIntermediateBalances(blockchain, resultDiff, resultDiff.scriptsComplexity, log)

            _ = invocationRoot.setResult(scriptResult)
          } yield (
            resultDiff,
            evaluated,
            remainingActions1,
            remainingBalanceActions1,
            remainingAssetActions1,
            remainingPayments1,
            remainingData1,
            remainingDataSize1
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
      invocation: ContractEvaluator.Invocation,
      environment: Environment[Id],
      limit: Int,
      startComplexityLimit: Int
  ): Coeval[Either[ValidationError, (ScriptResult, Log[Id])]] = {
    val evaluationCtx = CachedDAppCTX.get(version, blockchain).completeContext(environment)
    ContractEvaluator
      .applyV2Coeval(evaluationCtx, contract, invocation, version, limit, blockchain.correctFunctionCallScope, blockchain.newEvaluatorMode)
      .map(
        _.leftMap[ValidationError] {
          case (reject @ FailOrRejectError(_, true), _, _) =>
            reject.copy(skipInvokeComplexity = false)
          case (error, unusedComplexity, log) =>
            val usedComplexity = startComplexityLimit - unusedComplexity
            FailedTransactionError.dAppExecution(error.message, usedComplexity, log)
        }.flatTap { case (r, log) =>
          InvokeDiffsCommon
            .checkScriptResultFields(blockchain, r)
            .leftMap[ValidationError]({
              case reject: FailOrRejectError => reject
              case error =>
                val usedComplexity = startComplexityLimit - r.unusedComplexity
                FailedTransactionError.dAppExecution(error.toString, usedComplexity, log)
            })
        }
      )
  }

  private def validateIntermediateBalances(blockchain: Blockchain, diff: Diff, spentComplexity: Long, log: Log[Id]) = traced(
    if (blockchain.isFeatureActivated(BlockchainFeatures.RideV6)) {
      BalanceDiffValidation(blockchain)(diff)
        .leftMap { be => FailedTransactionError.dAppExecution(be.toString, spentComplexity, log) }
    } else if (blockchain.height >= blockchain.settings.functionalitySettings.enforceTransferValidationAfter) {
      // reject transaction if any balance is negative
      val compositeBlockchain = CompositeBlockchain(blockchain, diff)

      diff.portfolios.view
        .flatMap {
          case (address, p) if p.balance < 0 && compositeBlockchain.balance(address) < 0 => Some(address -> Waves)
          case (address, p) =>
            p.assets
              .find({ case (asset, balance) => balance < 0 && compositeBlockchain.balance(address, asset) < 0 })
              .map { case (asset, _) => address -> asset }
        }
        .headOption
        .fold[Either[ValidationError, Unit]](Right(())) { case (address, asset) =>
          val msg = asset match {
            case Waves => s"$address: Negative waves balance: old = ${blockchain.balance(address)}, new = ${compositeBlockchain.balance(address)}"
            case ia: IssuedAsset =>
              s"$address: Negative asset $ia balance: old = ${blockchain.balance(address, ia)}, new = ${compositeBlockchain.balance(address, ia)}"
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

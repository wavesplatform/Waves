package com.wavesplatform.state.diffs.invoke

import scala.util.Right
import cats.Id
import cats.instances.list.*
import cats.syntax.either.*
import cats.syntax.semigroup.*
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
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, IncompleteResult, Log, ScriptResult, ScriptResultV3, ScriptResultV4}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.unit
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.{Recipient as RideRecipient, *}
import com.wavesplatform.lang.v1.traits.domain.Tx.ScriptTransfer
import com.wavesplatform.metrics.*
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.invoke.CallArgumentPolicy.*
import com.wavesplatform.state.diffs.BalanceDiffValidation
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.{TransactionType, TxValidationError}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.*
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{DApp as DAppTarget, *}
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.{AssetVerifierTrace, CoevalR, TracedResult}
import com.wavesplatform.transaction.smart.script.trace.CoevalR.traced
import com.wavesplatform.transaction.validation.impl.DataTxValidator
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
      remainingData: Int,
      remainingDataSize: Int,
      calledAddresses: Set[Address],
      invocationRoot: DAppEnvironment.InvocationTreeTracker
  )(
      tx: InvokeScript
  ): CoevalR[(Diff, EVALUATED, Int, Int, Int)] = {
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
          _ <- traced {
            if (blockchain.height >= blockchain.settings.functionalitySettings.forbidSyncDAppNegativePaymentHeight) {
              tx.payments.find { case Payment(amount, _) =>
                amount < 0
              }.map(payment => Left(GenericError(s"DApp $invoker invoked DApp $dAppAddress with attached ${payment.assetId.fold("WAVES")(a => s"token $a")} amount = ${payment.amount}")))
                .getOrElse(Right(()))
            } else {
              Right(())
            }
          }
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
          payments                 <- traced(AttachedPaymentExtractor.extractPayments(tx, version, blockchain, DAppTarget).leftMap(GenericError.apply))
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
              val pseudoTx = ScriptTransfer(
                Some(assetId),
                RideRecipient.Address(ByteStr(tx.sender.toAddress.bytes)),
                tx.sender,
                RideRecipient.Address(ByteStr(tx.dApp.bytes)),
                amount,
                tx.timestamp,
                tx.txId
              )
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
                  val err = FailedTransactionError.assetExecutionInAction(error, totalComplexity, log, assetId)
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
            (diff, (scriptResult, log), availableActions, availableData, availableDataSize) <- {
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
                val paymentsPart                                    = InvokeDiffsCommon.paymentsPart(tx, tx.dApp, Map())
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
                  remainingData,
                  remainingDataSize,
                  paymentsPartInsideDApp,
                  invocationRoot
                )

                CoevalR(
                  evaluateV2(
                    version,
                    blockchain,
                    contract,
                    invocation,
                    environment,
                    complexityAfterPayments,
                    remainingComplexity
                  ).map(TracedResult(_))
                ).map(
                  result =>
                    (
                      environment.currentDiff |+| paymentsPartToResolve,
                      result,
                      environment.availableActions,
                      environment.availableData,
                      environment.availableDataSize
                    )
                )
              })
            }
            _ = invocationRoot.setLog(log)
            spentComplexity = remainingComplexity - scriptResult.unusedComplexity.max(0)

            _ <- traced(
              BalanceDiffValidation
                .cond(blockchain, b => b.isFeatureActivated(BlockchainFeatures.SynchronousCalls) && b.height >= b.settings.functionalitySettings.enforceTransferValidationAfter)(diff)
                .leftMap(be => FailedTransactionError.dAppExecution(be.toString, spentComplexity, log))
            )

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

            process = { (actions: List[CallableAction], unusedComplexity: Int, actionsCount: Int, dataCount: Int, dataSize: Int, ret: EVALUATED) =>
              for {
                _ <- CoevalR(
                  Coeval(
                    InvokeDiffsCommon.checkCallResultLimits(
                      blockchain,
                      remainingComplexity - unusedComplexity,
                      log,
                      actionsCount,
                      dataCount,
                      dataSize,
                      availableActions,
                      availableData,
                      availableDataSize
                    )
                  )
                )
                diff <- doProcessActions(actions, unusedComplexity)
              } yield (diff, ret, availableActions - actionsCount, availableData - dataCount, availableDataSize - dataSize)
            }

            (actionsDiff, evaluated, remainingActions1, remainingData1, remainingDataSize1) <- scriptResult match {
              case ScriptResultV3(dataItems, transfers, unusedComplexity) =>
                val dataEntries  = dataItems.map(InvokeDiffsCommon.dataItemToEntry)
                val dataCount    = dataItems.length
                val dataSize     = DataTxValidator.invokeWriteSetSize(blockchain, dataEntries)
                val actionsCount = transfers.length
                process(dataItems ::: transfers, unusedComplexity, actionsCount, dataCount, dataSize, unit)
              case ScriptResultV4(actions, unusedComplexity, ret) =>
                val dataEntries  = actions.collect { case d: DataOp => InvokeDiffsCommon.dataItemToEntry(d) }
                val dataCount    = dataEntries.length
                val dataSize     = DataTxValidator.invokeWriteSetSize(blockchain, dataEntries)
                val actionsCount = actions.length - dataCount
                process(actions, unusedComplexity, actionsCount, dataCount, dataSize, ret)
              case _: IncompleteResult if limitedExecution =>
                doProcessActions(Nil, 0).map((_, unit, availableActions, availableData, availableDataSize))
              case r: IncompleteResult =>
                val usedComplexity = remainingComplexity - r.unusedComplexity
                val error          = FailedTransactionError.dAppExecution(s"Invoke complexity limit = $totalComplexityLimit is exceeded", usedComplexity, log)
                traced(error.asLeft[(Diff, EVALUATED, Int, Int, Int)])
            }
            resultDiff = diff.copy(scriptsComplexity = 0) |+| actionsDiff |+| Diff.empty.copy(scriptsComplexity = paymentsComplexity)

            _ <- traced(
              BalanceDiffValidation
                .cond(blockchain, b => b.isFeatureActivated(BlockchainFeatures.SynchronousCalls) && b.height >= b.settings.functionalitySettings.enforceTransferValidationAfter)(resultDiff)
                .leftMap(be => FailedTransactionError.dAppExecution(be.toString, resultDiff.scriptsComplexity, log))
            )

            _ = invocationRoot.setResult(scriptResult)
          } yield (resultDiff, evaluated, remainingActions1, remainingData1, remainingDataSize1)
        } yield result

      case _ => traced(Left(GenericError(s"No contract at address ${tx.dApp}")))
    }

    result.leftMap { err =>
      invocationRoot.setError(err)
      err
    }
  }

  private def evaluateV2(
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
        _.leftMap(
          {
            case (error, unusedComplexity, log) =>
              val usedComplexity = startComplexityLimit - unusedComplexity
              FailedTransactionError.dAppExecution(error, usedComplexity, log)
          }
        ).flatMap {
          case (result, log) =>
            val usedComplexity = startComplexityLimit - result.unusedComplexity.max(0)
            (for (_ <-
            InvokeDiffsCommon.checkScriptResultFields(blockchain, result)) yield (result, log))
              .leftMap(err => FailedTransactionError.dAppExecution(err.toString, usedComplexity, log))
        }
      )
  }
}

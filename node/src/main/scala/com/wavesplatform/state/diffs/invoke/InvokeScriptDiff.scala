package com.wavesplatform.state.diffs.invoke

import cats.Id
import cats.implicits._
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.EstimatorProvider.EstimatorBlockchainExt
import com.wavesplatform.lang._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.unit
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
import com.wavesplatform.transaction.smart.script.trace.CoevalR.traced
import com.wavesplatform.transaction.smart.script.trace.{AssetVerifierTrace, CoevalR, TracedResult}
import com.wavesplatform.transaction.smart.{DApp => DAppTarget, _}
import com.wavesplatform.transaction.{Transaction, TxValidationError}
import monix.eval.Coeval
import shapeless.Coproduct

import scala.util.Right

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
      calledAddresses: Set[Address],
      invocationRoot: DAppEnvironment.InvocationTreeTracker
  )(
      tx: InvokeScript
  ): CoevalR[(Diff, EVALUATED, Int, Int)] = {
    val dAppAddress = tx.dAppAddress
    val invoker     = tx.senderDApp

    val result = blockchain.accountScript(dAppAddress) match {
      case Some(AccountScriptInfo(pk, ContractScriptImpl(version, contract), _, callableComplexities)) =>
        for {
          _ <- traced(
            Either.cond(
              version >= V5,
              (),
              GenericError(s"DApp $invoker invoked DApp $dAppAddress that uses RIDE $version, but dApp-to-dApp invocation requires version 5 or higher")
            )
          )
          _ <- traced(
            Either.cond(
              remainingCalls > 0,
              (),
              ValidationError.ScriptRunsLimitError(s"DApp calls limit = ${ContractLimits.MaxSyncDAppCalls(version)} is exceeded")
            )
          )
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
            case (error @ TracedResult(Left(_), _), _) => error
            case (TracedResult(Right(nextRemainingComplexity), _), (script, amount, assetId)) =>
              val usedComplexity = totalComplexityLimit - nextRemainingComplexity
              val pseudoTx = ScriptTransfer(
                Some(assetId),
                Recipient.Address(ByteStr(tx.senderDApp.bytes)),
                tx.sender,
                Recipient.Address(ByteStr(tx.dAppAddress.bytes)),
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

          tthis = Coproduct[Environment.Tthis](Recipient.Address(ByteStr(dAppAddress.bytes)))
          input <- traced(
            tx.root
              .map(t => buildThisValue(Coproduct[TxOrd](t: Transaction), blockchain, directives, tthis).leftMap(GenericError.apply))
              .getOrElse(Right(null))
          )

          result <- for {
            (diff, (scriptResult, log), availableActions, availableData) <- {
              stats.invokedScriptExecution.measureForType(InvokeScriptTransaction.typeId)({
                val height = blockchain.height
                val invocation = ContractEvaluator.Invocation(
                  tx.funcCall,
                  Recipient.Address(ByteStr(invoker.bytes)),
                  ByteStr(tx.sender.arr),
                  Recipient.Address(ByteStr(tx.root.fold(invoker)(_.senderAddress).bytes)),
                  ByteStr(tx.root.getOrElse(tx).sender.arr),
                  payments,
                  tx.txId,
                  tx.root.map(_.fee).getOrElse(0L),
                  tx.root.flatMap(_.feeAssetId.compatId)
                )
                val paymentsPart                                    = InvokeDiffsCommon.paymentsPart(tx, tx.dAppAddress, Map())
                val (paymentsPartInsideDApp, paymentsPartToResolve) = if (version < V5) (Diff.empty, paymentsPart) else (paymentsPart, Diff.empty)
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
                  calledAddresses,
                  limitedExecution,
                  totalComplexityLimit,
                  remainingCalls - 1,
                  remainingActions,
                  remainingData,
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
                ).map(result => (environment.currentDiff |+| paymentsPartToResolve, result, environment.availableActions, environment.availableData))
              })
            }

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

            process = { (actions: List[CallableAction], unusedComplexity: Int, actionsCount: Int, dataCount: Int, ret: EVALUATED) =>
              if (dataCount > availableData) {
                val usedComplexity = remainingComplexity - unusedComplexity
                val error          = FailedTransactionError.dAppExecution("Stored data count limit is exceeded", usedComplexity, log)
                traced(error.asLeft[(Diff, EVALUATED, Int, Int)])
              } else {
                if (actionsCount > availableActions) {
                  val usedComplexity = remainingComplexity - unusedComplexity
                  val error          = FailedTransactionError.dAppExecution("Actions count limit is exceeded", usedComplexity, log)
                  traced(error.asLeft[(Diff, EVALUATED, Int, Int)])
                } else {
                  doProcessActions(actions, unusedComplexity).map((_, ret, availableActions - actionsCount, availableData - dataCount))
                }
              }
            }
            (actionsDiff, evaluated, remainingActions1, remainingData1) <- scriptResult match {
              case ScriptResultV3(dataItems, transfers, unusedComplexity) =>
                val dataCount    = dataItems.length
                val actionsCount = transfers.length
                process(dataItems ::: transfers, unusedComplexity, actionsCount, dataCount, unit)
              case ScriptResultV4(actions, unusedComplexity, ret) =>
                val dataCount    = actions.count(_.isInstanceOf[DataOp])
                val actionsCount = actions.length - dataCount
                process(actions, unusedComplexity, actionsCount, dataCount, ret)
              case _: IncompleteResult if limitedExecution => doProcessActions(Nil, 0).map((_, unit, availableActions, availableData))
              case r: IncompleteResult =>
                val usedComplexity = remainingComplexity - r.unusedComplexity
                val error          = FailedTransactionError.dAppExecution(s"Invoke complexity limit = $totalComplexityLimit is exceeded", usedComplexity, log)
                traced(error.asLeft[(Diff, EVALUATED, Int, Int)])
            }
            resultDiff = diff.copy(scriptsComplexity = 0) |+| actionsDiff |+| Diff.empty.copy(scriptsComplexity = paymentsComplexity)
          } yield (resultDiff, evaluated, remainingActions1, remainingData1)
        } yield result

      case _ => traced(Left(GenericError(s"No contract at address ${tx.dAppAddress}")))
    }
    result
  }

  private def evaluateV2(
      version: StdLibVersion,
      blockchain: Blockchain,
      contract: DApp,
      invocation: ContractEvaluator.Invocation,
      environment: Environment[Id],
      limit: Int,
      startComplexity: Int
  ): Coeval[Either[ValidationError, (ScriptResult, Log[Id])]] = {
    val evaluationCtx = CachedDAppCTX.get(version, blockchain).completeContext(environment)
    ContractEvaluator
      .applyV2Coeval(evaluationCtx, Map(), contract, invocation, version, limit)
      .map(
        _.leftMap(
          {
            case (error, unusedComplexity, log) =>
              val usedComplexity = startComplexity - unusedComplexity
              FailedTransactionError.dAppExecution(error, usedComplexity, log)
          }
        )
      )
  }
}

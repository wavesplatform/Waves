package com.wavesplatform.state.diffs.invoke

import scala.util.Right

import cats.Id
import cats.implicits._
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, IncompleteResult, Log, ScriptResult, ScriptResultV3, ScriptResultV4}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.unit
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain._
import com.wavesplatform.lang.v1.traits.domain.Tx.ScriptTransfer
import com.wavesplatform.metrics._
import com.wavesplatform.state._
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.{Transaction, TxValidationError}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction.smart.{DApp => DAppTarget, _}
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.{AssetVerifierTrace, CoevalR, TracedResult}
import com.wavesplatform.transaction.smart.script.trace.CoevalR.traced
import monix.eval.Coeval
import shapeless.Coproduct

object InvokeScriptDiff {
  private val stats = TxProcessingStats
  import stats.TxTimerExt

  def apply(
      blockchain: Blockchain,
      blockTime: Long,
      limitedExecution: Boolean,
      remainingComplexity: Int,
      remainingCalls: Int,
      remainingActions: Int,
      remainingData: Int,
      invocationChain: Seq[DAppEnvironment.DAppInvocation],
      calledAddresses: Set[Address]
  )(
      tx: InvokeScript
  ): (CoevalR[(Diff, EVALUATED, Int, Int)], Seq[DAppEnvironment.DAppInvocation]) = {
    val dAppAddress = tx.dAppAddress
    val invoker     = tx.senderDApp

    val invocation = DAppEnvironment.DAppInvocation(dAppAddress, tx.funcCall, tx.payments)
    val result = blockchain.accountScript(dAppAddress) match {
      case Some(AccountScriptInfo(pk, ContractScriptImpl(version, contract), _, callableComplexities)) =>
        val limit = ContractLimits.MaxTotalInvokeComplexity(version)
        for {
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
          complexityAfterPayments <- checkedPayments.foldLeft(traced(Right(remainingComplexity): TxValidationError.Validation[Int])) { (prev, a) =>
            (prev.v(), a) match {
              case (TracedResult(Left(_), _), _) => prev
              case (TracedResult(Right(nextRemainingComplexity), _), (script, amount, assetId)) =>
                val usedComplexity = limit - nextRemainingComplexity
                val r = if (script.complexity > nextRemainingComplexity) {
                  val err = FailedTransactionError.assetExecution(s"Invoke complexity limit = $limit is exceeded", usedComplexity, Nil, assetId)
                  TracedResult(Left(err), List(AssetVerifierTrace(assetId, Some(err))))
                } else {
                  val pseudoTx: PseudoTx = ScriptTransfer(
                    Some(assetId),
                    Recipient.Address(ByteStr(tx.senderDApp.bytes)),
                    tx.sender,
                    Recipient.Address(ByteStr(tx.dAppAddress.bytes)),
                    amount,
                    tx.timestamp,
                    tx.txId
                  )
                  ScriptRunner(
                    Coproduct[TxOrd](pseudoTx),
                    blockchain,
                    script.script,
                    isAssetScript = true,
                    scriptContainerAddress = Coproduct[Environment.Tthis](Environment.AssetId(assetId.arr)),
                    Int.MaxValue
                  ) match {
                    case (log, Left(error)) =>
                      val err =
                        FailedTransactionError.assetExecutionInAction(s"Invoke complexity limit = $limit is exceeded", usedComplexity, log, assetId)
                      TracedResult(Left(err), List(AssetVerifierTrace(assetId, Some(err))))
                    case (log, Right(FALSE)) =>
                      val err = FailedTransactionError.notAllowedByAsset(usedComplexity, log, assetId)
                      TracedResult(Left(err), List(AssetVerifierTrace(assetId, Some(err))))
                    case (_, Right(TRUE)) =>
                      TracedResult(Right(nextRemainingComplexity - script.complexity.toInt))
                    case (log, Right(x)) =>
                      val err = FailedTransactionError.assetExecution(s"Script returned not a boolean result, but $x", usedComplexity, log, assetId)
                      TracedResult(Left(err), List(AssetVerifierTrace(assetId, Some(err))))
                  }
                }
                CoevalR(Coeval.now(r))
            }
          }
          paymentsComplexity = checkedPayments.map(_._1.complexity).sum.toInt

          tthis = Coproduct[Environment.Tthis](Recipient.Address(ByteStr(dAppAddress.bytes)))
          input <- traced(
            tx.root
              .map(t => buildThisValue(Coproduct[TxOrd](t: Transaction), blockchain, directives, tthis).leftMap(GenericError.apply))
              .getOrElse(Right(null))
          )

          result <- for {
            (diff, (scriptResult, log), avaliableActions, avaliableData) <- {
              val scriptResultE = stats.invokedScriptExecution.measureForType(InvokeScriptTransaction.typeId)({
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
                  calledAddresses + dAppAddress,
                  limitedExecution,
                  remainingCalls - 1,
                  remainingActions,
                  remainingData,
                  (if (version < V5) {
                     Diff.empty
                   } else {
                     InvokeDiffsCommon.paymentsPart(tx, tx.dAppAddress, Map())
                   })
                )

                for {
                  result <- CoevalR(
                    evaluateV2(
                      version,
                      contract,
                      ContractEvaluator.Invocation(
                        tx.funcCall,
                        Recipient.Address(ByteStr(invoker.bytes)),
                        ByteStr(tx.sender.arr),
                        Recipient.Address(ByteStr(tx.root.fold(invoker)(_.senderAddress).bytes)),
                        ByteStr(tx.root.getOrElse(tx).sender.arr),
                        AttachedPayments.Multi(tx.payments.map(p => p.amount -> p.assetId.compatId)),
                        tx.txId,
                        tx.root.map(_.fee).getOrElse(0L),
                        tx.root.flatMap(_.feeAssetId.compatId)
                      ),
                      environment,
                      complexityAfterPayments,
                      remainingComplexity
                    ).map(TracedResult(_))
                  )
                } yield (environment.currentDiff, result, environment.availableActions, environment.availableData)
              })
              scriptResultE
            }

            doProcessActions = (actions: List[CallableAction], unusedComplexity: Int) =>
              CoevalR(
                Coeval.now(
                  InvokeDiffsCommon.processActions(
                    actions,
                    version,
                    dAppAddress,
                    pk,
                    invocationComplexity,
                    tx,
                    CompositeBlockchain(blockchain, Some(diff)),
                    blockTime,
                    unusedComplexity,
                    isSyncCall = true,
                    limitedExecution,
                    Seq()
                  )
                )
              )

            process = { (actions: List[CallableAction], unusedComplexity: Int, actionsCount: Int, dataCount: Int, ret: EVALUATED) =>
              if (dataCount > avaliableData) {
                val usedComplexity = remainingComplexity - unusedComplexity
                val error          = FailedTransactionError.dAppExecution("Stored data count limit is exceeded", usedComplexity, log)
                traced(error.asLeft[(Diff, EVALUATED, Int, Int)])
              } else {
                if (actionsCount > avaliableActions) {
                  val usedComplexity = remainingComplexity - unusedComplexity
                  val error          = FailedTransactionError.dAppExecution("Actions count limit is exceeded", usedComplexity, log)
                  traced(error.asLeft[(Diff, EVALUATED, Int, Int)])
                } else {
                  doProcessActions(actions, unusedComplexity).map((_, ret, avaliableActions - actionsCount, avaliableData - dataCount))
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
              case _: IncompleteResult if limitedExecution => doProcessActions(Nil, 0).map((_, unit, avaliableActions, avaliableData))
              case r: IncompleteResult =>
                val usedComplexity = remainingComplexity - r.unusedComplexity
                val error          = FailedTransactionError.dAppExecution(s"Invoke complexity limit = $limit is exceeded", usedComplexity, log)
                traced(error.asLeft[(Diff, EVALUATED, Int, Int)])
            }
            resultDiff = diff |+| actionsDiff |+| Diff.empty.copy(scriptsComplexity = paymentsComplexity)
          } yield (resultDiff, evaluated, remainingActions1, remainingData1)
        } yield result

      case _ => traced(Left(GenericError(s"No contract at address ${tx.dAppAddress}")))
    }
    (result, invocationChain :+ invocation)
  }

  private def evaluateV2(
      version: StdLibVersion,
      contract: DApp,
      invocation: ContractEvaluator.Invocation,
      environment: Environment[Id],
      limit: Int,
      startComplexity: Int
  ): Coeval[Either[ValidationError, (ScriptResult, Log[Id])]] = {
    val evaluationCtx = CachedDAppCTX.forVersion(version).completeContext(environment)
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

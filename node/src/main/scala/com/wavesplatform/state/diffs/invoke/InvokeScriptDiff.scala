package com.wavesplatform.state.diffs.invoke

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

  def apply(blockchain: Blockchain, blockTime: Long, limitedExecution: Boolean, remainingComplexity: Int, remainingCalls: Int, callsLimit: Int)(
      tx: InvokeScript
  ): CoevalR[(Diff, EVALUATED)] = {
    val dAppAddress = tx.dAppAddress
    blockchain.accountScript(dAppAddress) match {
      case Some(AccountScriptInfo(pk, ContractScriptImpl(version, contract), _, callableComplexities)) =>
        for {
          _ <- traced(
            Either.cond(
              remainingCalls > 0,
              (),
              ValidationError.ScriptRunsLimitError(s"DApp calls limit = $callsLimit for attached fee = ${tx.root.fee} is exceeded")
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
          _ <- checkedPayments.foldLeft(traced(Right(()): TxValidationError.Validation[Unit])) { (prev, a) =>
            (prev.v.value(), a) match {
              case (TracedResult(Left(_), _), _) => prev
              case (_, (script, amount, assetId)) =>
                val pseudoTx: PseudoTx = ScriptTransfer(
                  Some(assetId),
                  Recipient.Address(ByteStr(tx.senderDApp.bytes)),
                  tx.sender,
                  Recipient.Address(ByteStr(tx.dAppAddress.bytes)),
                  amount,
                  tx.root.timestamp,
                  tx.root.id()
                )
                val r = ScriptRunner(
                  Coproduct[TxOrd](pseudoTx),
                  blockchain,
                  script.script,
                  isAssetScript = true,
                  scriptContainerAddress = Coproduct[Environment.Tthis](Environment.AssetId(assetId.arr)),
                  Int.MaxValue
                ) match {
                  case (log, Left(error)) =>
                    val err = FailedTransactionError.assetExecutionInAction(error, 0, log, assetId)
                    TracedResult(Left(err), List(AssetVerifierTrace(assetId, Some(err))))
                  case (log, Right(FALSE)) =>
                    val err = FailedTransactionError.notAllowedByAssetInAction(0, log, assetId)
                    TracedResult(Left(err), List(AssetVerifierTrace(assetId, Some(err))))
                  case (_, Right(TRUE)) =>
                    TracedResult(Right(()))
                  case (log, Right(x)) =>
                    val err = FailedTransactionError.assetExecutionInAction(s"Script returned not a boolean result, but $x", 0, log, assetId)
                    TracedResult(Left(err), List(AssetVerifierTrace(assetId, Some(err))))
                }
                CoevalR(Coeval.now(r))
            }
          }
          tthis = Coproduct[Environment.Tthis](Recipient.Address(ByteStr(dAppAddress.bytes)))
          input <- traced(
            buildThisValue(Coproduct[TxOrd](tx.root: Transaction), blockchain, directives, tthis).leftMap(GenericError.apply)
          )

          result <- for {
            scriptResult <- {
              val scriptResultE = stats.invokedScriptExecution.measureForType(InvokeScriptTransaction.typeId)({
                val invoker = tx.senderDApp
                val invocation = ContractEvaluator.Invocation(
                  tx.funcCall,
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
                  callsLimit,
                  remainingCalls - 1
                )

                for {
                  result <- CoevalR(
                    evaluateV2(version, contract, invocation, environment, remainingComplexity).map(TracedResult(_))
                  )
                } yield (environment.currentDiff, result)
              })
              scriptResultE
            }

            doProcessActions = (actions: List[CallableAction]) =>
              CoevalR(
                Coeval.now(
                  InvokeDiffsCommon.processActions(
                    actions,
                    version,
                    dAppAddress,
                    pk,
                    invocationComplexity,
                    tx,
                    CompositeBlockchain(blockchain, Some(scriptResult._1)),
                    blockTime,
                    remainingComplexity - scriptResult._1.scriptsComplexity - checkedPayments.map(_._1.complexity).sum,
                    isSyncCall = true,
                    limitedExecution,
                    Seq()
                  )
                )
              )

            resultDiff <- scriptResult._2._1 match {
              case ScriptResultV3(dataItems, transfers, _) => doProcessActions(dataItems ::: transfers).map(r => (r, unit))
              case ScriptResultV4(actions, _, ret)         => doProcessActions(actions).map(r => (r, ret))
              case _: IncompleteResult if limitedExecution => doProcessActions(Nil).map((_, unit))
              case r: IncompleteResult =>
                val limit          = ContractLimits.MaxTotalInvokeComplexity(version)
                val usedComplexity = remainingComplexity - r.unusedComplexity
                val error          = FailedTransactionError.dAppExecution(s"Invoke complexity limit = $limit is exceeded", usedComplexity, scriptResult._2._2)
                traced(error.asLeft[(Diff, EVALUATED)])
            }
          } yield resultDiff.copy(_1 = scriptResult._1 combine resultDiff._1)
        } yield result

      case _ => traced(Left(GenericError(s"No contract at address ${tx.dAppAddress}")))
    }
  }

  private def evaluateV2(
      version: StdLibVersion,
      contract: DApp,
      invocation: ContractEvaluator.Invocation,
      environment: Environment[Id],
      limit: Int
  ): Coeval[Either[ValidationError, (ScriptResult, Log[Id])]] = {
    val evaluationCtx = CachedDAppCTX.forVersion(version).completeContext(environment)
    ContractEvaluator
      .applyV2Coeval(evaluationCtx, Map(), contract, invocation, version, limit)
      .map(
        _.leftMap(
          {
            case (error, unusedComplexity, log) =>
              val usedComplexity = limit - unusedComplexity
              FailedTransactionError.dAppExecution(error, usedComplexity, log)
          }
        )
      )
  }
}

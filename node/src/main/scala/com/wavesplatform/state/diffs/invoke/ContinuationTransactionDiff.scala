package com.wavesplatform.state.diffs.invoke

import cats.Id
import cats.implicits._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, DApp}
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.evaluator.ctx.LazyVal
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, IncompleteResult, ScriptResultV3, ScriptResultV4}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.Environment.Tthis
import com.wavesplatform.lang.v1.traits.domain.{CallableAction, Recipient}
import com.wavesplatform.lang.{Global, ValidationError}
import com.wavesplatform.state.{AccountScriptInfo, Blockchain, ContinuationState, Diff, NewTransactionInfo}
import com.wavesplatform.transaction.ApplicationStatus.Succeeded
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError.{FailedTransactionError, GenericError}
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.{InvokeScriptTrace, TracedResult}
import com.wavesplatform.transaction.smart.{ContinuationTransaction, WavesEnvironment, buildThisValue}
import monix.eval.Coeval
import shapeless.Coproduct

object ContinuationTransactionDiff {
  def apply(blockchain: Blockchain, blockTime: Long, limitedExecution: Boolean = true)(
      tx: ContinuationTransaction
  ): TracedResult[ValidationError, Diff] = {
    blockchain.continuationStates(tx.invokeScriptTransactionId) match {
      case (_, ContinuationState.InProgress(expr, residualComplexity, _)) =>
        applyDiff(blockchain, blockTime, limitedExecution, tx, expr, residualComplexity)
      case _ =>
        TracedResult.wrapValue(Diff.empty)
    }
  }

  private def applyDiff(
      blockchain: Blockchain,
      blockTime: Long,
      limitedExecution: Boolean,
      tx: ContinuationTransaction,
      expr: Terms.EXPR,
      residualComplexity: Int
  ): TracedResult[ValidationError, Diff] = {
    val invoke = blockchain.resolveInvoke(tx)
    for {
      dAppAddress <- TracedResult(blockchain.resolveAlias(invoke.dAppAddressOrAlias))
      scriptInfo <- TracedResult(
        blockchain
          .accountScript(dAppAddress)
          .toRight(GenericError(s"Cannot find dApp with address=$dAppAddress"))
      )
      AccountScriptInfo(dAppPublicKey, script, _, callableComplexities) = scriptInfo
      directives <- TracedResult(
        DirectiveSet(script.stdLibVersion, Account, DApp).left.map(GenericError(_))
      )

      rideAddress = Coproduct[Tthis](Recipient.Address(ByteStr(dAppAddress.bytes)))
      input <- TracedResult(
        buildThisValue(Coproduct[TxOrd](invoke: Transaction), blockchain, directives, rideAddress).left.map(GenericError(_))
      )

      invokeHeight = blockchain.transactionMeta(invoke.id.value()).get._1
      environment = new WavesEnvironment(
        AddressScheme.current.chainId,
        Coeval.evalOnce(input),
        Coeval(invokeHeight),
        blockchain,
        rideAddress,
        directives,
        tx.invokeScriptTransactionId
      )
      scriptResult <- {
        val ctx =
          PureContext.build(script.stdLibVersion).withEnvironment[Environment] |+|
            CryptoContext.build(Global, script.stdLibVersion).withEnvironment[Environment] |+|
            WavesContext.build(directives).copy(vars = Map())

        val limit = ContractLimits.MaxComplexityByVersion(script.stdLibVersion) + residualComplexity
        val r = ContractEvaluator
          .applyV2(
            ctx.evaluationContext(environment),
            Map[String, LazyVal[Id]](),
            expr,
            script.stdLibVersion,
            tx.invokeScriptTransactionId,
            limit,
            continuationFirstStepMode = false
          )
          .leftMap { case (error, log) => FailedTransactionError.dAppExecution(error, 0, log) }
        TracedResult(
          r,
          List(InvokeScriptTrace(invoke.dAppAddressOrAlias, invoke.funcCall, r.map(_._1), r.fold(_.log, _._2)))
        )
      }

      invocationComplexity <- TracedResult(
        InvokeDiffsCommon.getInvocationComplexity(blockchain, invoke, callableComplexities, dAppAddress)
      )

      doProcessActions = InvokeDiffsCommon
        .processActions(
          _: List[CallableAction],
          script.stdLibVersion,
          dAppAddress,
          dAppPublicKey,
          invocationComplexity,
          invoke,
          blockchain,
          blockTime,
          useFeeDiff = false,
          limitedExecution
        )
        .map(InvokeDiffsCommon.finishContinuation(_, tx, blockchain, invoke, failed = false).copy(transactions = Map()))

      resultDiff <- scriptResult._1 match {
        case ScriptResultV3(dataItems, transfers) =>
          doProcessActions(dataItems ::: transfers)
        case ScriptResultV4(actions) =>
          doProcessActions(actions)
        case ir: IncompleteResult =>
          val newState = ContinuationState.InProgress(ir.expr, ir.unusedComplexity, tx.id.value())
          val stepFee  = InvokeDiffsCommon.stepTotalFee(Diff.empty, blockchain, invoke)
          TracedResult.wrapValue[Diff, ValidationError](
            Diff.stateOps(
              continuationStates = Map((tx.invokeScriptTransactionId, tx.nonce + 1) -> newState),
              replacingTransactions = Seq(NewTransactionInfo(tx.copy(fee = stepFee), Set(), Succeeded)),
              portfolios = InvokeDiffsCommon.stepFeePortfolios(stepFee, invoke, blockchain)
            )
          )
      }
    } yield resultDiff
  }
}

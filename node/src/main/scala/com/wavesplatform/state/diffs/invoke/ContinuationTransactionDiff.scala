package com.wavesplatform.state.diffs.invoke

import cats.Id
import cats.implicits._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, DApp}
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.evaluator.ctx.LazyVal
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, IncompleteResult, ScriptResultV3, ScriptResultV4}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.Environment.Tthis
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.lang.{Global, ValidationError}
import com.wavesplatform.state.{AccountScriptInfo, Blockchain, ContinuationState, Diff}
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError.{GenericError, ScriptExecutionError}
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.{InvokeScriptTrace, TracedResult}
import com.wavesplatform.transaction.smart.{ContinuationTransaction, InvokeScriptTransaction, WavesEnvironment, buildThisValue}
import monix.eval.Coeval
import shapeless.Coproduct

object ContinuationTransactionDiff {
  def apply(blockchain: Blockchain, blockTime: Long, verifyAssets: Boolean = true)(tx: ContinuationTransaction): TracedResult[ValidationError, Diff] = {
    val (invokeHeight, foundTx, status) = blockchain.transactionInfo(tx.invokeScriptTransactionId).get
    val invokeScriptTransaction         = foundTx.asInstanceOf[InvokeScriptTransaction]
    for {
      _ <- TracedResult(
        Either.cond(status, (), GenericError(s"Cannot continue failed invoke script transaction with id=${tx.invokeScriptTransactionId}"))
      )
      dAppAddress <- TracedResult(blockchain.resolveAlias(invokeScriptTransaction.dAppAddressOrAlias))
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
        buildThisValue(Coproduct[TxOrd](invokeScriptTransaction: Transaction), blockchain, directives, rideAddress).left.map(GenericError(_))
      )

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

        val ContinuationState.InProgress(_, expr, residualComplexity) =
          blockchain.continuationStates(tx.invokeScriptTransactionId).asInstanceOf[ContinuationState.InProgress]

        val limit = ContractLimits.MaxComplexityByVersion(script.stdLibVersion) + residualComplexity
        val r = ContractEvaluator
          .applyV2(ctx.evaluationContext(environment), Map[String, LazyVal[Id]](), expr, script.stdLibVersion, tx.invokeScriptTransactionId, limit)
          .leftMap { case (error, log) => ScriptExecutionError.dAppExecution(error, log) }
        TracedResult(
          r,
          List(InvokeScriptTrace(invokeScriptTransaction.dAppAddressOrAlias, invokeScriptTransaction.funcCall, r.map(_._1), r.fold(_.log, _._2)))
        )
      }

      invocationComplexity <- TracedResult(
        InvokeDiffsCommon.getInvocationComplexity(blockchain, invokeScriptTransaction, callableComplexities, dAppAddress)
      )

      feeInfo <- TracedResult(InvokeDiffsCommon.calcFee(blockchain, invokeScriptTransaction))

      verifierComplexity = blockchain.accountScript(invokeScriptTransaction.sender.toAddress).map(_.verifierComplexity).getOrElse(0L)

      doProcessActions = InvokeDiffsCommon.processActions(
        _,
        script.stdLibVersion,
        dAppAddress,
        dAppPublicKey,
        feeInfo,
        invocationComplexity,
        invokeScriptTransaction,
        blockchain,
        blockTime,
        verifyAssets
      )

      continuationStopDiff = Diff.stateOps(continuationStates = Map(tx.invokeScriptTransactionId -> ContinuationState.Finished))

      resultDiff <- scriptResult._1 match {
        case ScriptResultV3(dataItems, transfers) =>
          doProcessActions(dataItems ::: transfers).map(_.copy(transactions = Map()) |+| continuationStopDiff)
        case ScriptResultV4(actions) =>
          doProcessActions(actions).map(_.copy(transactions = Map()) |+| continuationStopDiff)
        case ir: IncompleteResult =>
          TracedResult.wrapValue[Diff, ValidationError](
            Diff.stateOps(continuationStates = Map(tx.invokeScriptTransactionId -> ContinuationState.InProgress(tx.nonce + 1, ir.expr, ir.unusedComplexity)))
          )
      }

    } yield resultDiff
  }
}

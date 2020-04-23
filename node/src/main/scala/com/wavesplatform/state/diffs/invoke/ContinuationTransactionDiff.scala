package com.wavesplatform.state.diffs.invoke

import cats.implicits._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, DApp}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, IncompleteResult, ScriptResultV3, ScriptResultV4}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.{Global, ValidationError}
import com.wavesplatform.state.{AccountScriptInfo, Blockchain, Diff}
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.{ContinuationTransaction, InvokeScriptTransaction, WavesEnvironment, buildThisValue}
import monix.eval.Coeval
import shapeless.Coproduct

object ContinuationTransactionDiff {
  def apply(blockchain: Blockchain, blockTime: Long)(tx: ContinuationTransaction): Either[ValidationError, Diff] = {
    println(s"START CONTINUATION DIFF")
    val (invokeHeight, foundTx, status) = blockchain.transactionInfo(tx.invokeScriptTransactionId).get
    val invokeScriptTransaction = foundTx.asInstanceOf[InvokeScriptTransaction]
    for {
      dAppAddress <- blockchain.resolveAlias(invokeScriptTransaction.dAppAddressOrAlias)
      scriptInfo <- blockchain
        .accountScript(dAppAddress)
        .toRight(GenericError("ERROR"))
      AccountScriptInfo(dAppPublicKey, script, _, callableComplexities) = scriptInfo
      directives <- DirectiveSet(script.stdLibVersion, Account, DApp).leftMap(GenericError(_))

      ctx = PureContext.build(Global, script.stdLibVersion).withEnvironment[Environment] |+|
        CryptoContext.build(Global, script.stdLibVersion).withEnvironment[Environment] |+|
        WavesContext.build(directives)

      input <- buildThisValue(Coproduct[TxOrd](invokeScriptTransaction: Transaction), blockchain, directives, None).leftMap(GenericError(_))

      environment = new WavesEnvironment(
        AddressScheme.current.chainId,
        Coeval.evalOnce(input),
        Coeval(invokeHeight),
        blockchain,
        Coeval(dAppAddress.bytes),
        directives,
        tx.invokeScriptTransactionId
      )
      scriptResult <- ContractEvaluator.applyV2(ctx.evaluationContext(environment), tx.expr, script.stdLibVersion, tx.invokeScriptTransactionId).leftMap(GenericError(_))

      invocationComplexity <- InvokeDiffsCommon.getInvocationComplexity(blockchain, invokeScriptTransaction, callableComplexities, dAppAddress)

      feeInfo <- InvokeDiffsCommon.calcFee(blockchain, invokeScriptTransaction)

      verifierComplexity = blockchain.accountScript(invokeScriptTransaction.sender).map(_.verifierComplexity).getOrElse(0L)

      doProcessActions = InvokeDiffsCommon.processActions(
        _,
        script.stdLibVersion,
        dAppAddress,
        dAppPublicKey,
        feeInfo,
        invocationComplexity,
        verifierComplexity,
        invokeScriptTransaction,
        blockchain,
        blockTime
      )

      continuationStopDiff = Diff.stateOps(continuationStates = Map(tx.invokeScriptTransactionId -> null))

      resultDiff <- scriptResult match {
        case ScriptResultV3(dataItems, transfers) => doProcessActions(dataItems ::: transfers).resultE.map(_ |+| continuationStopDiff)
        case ScriptResultV4(actions)              => doProcessActions(actions).resultE.map(_ |+| continuationStopDiff)
        case ir: IncompleteResult                 => Right(Diff.stateOps(continuationStates = Map(tx.invokeScriptTransactionId -> ir.expr)))
      }

    } yield {
      println(s"CONTINUATION RESULT DIFF $resultDiff")
      resultDiff
    }
  }

/*  private def removeCurrentState(diff: Diff, id: ByteStr) = {
    diff.copy(continuationStates = diff.continuationStates - id)
  }*/
}

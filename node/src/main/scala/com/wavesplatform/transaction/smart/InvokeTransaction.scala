package com.wavesplatform.transaction.smart
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionLike
import com.wavesplatform.transaction.{Asset, FastHashId, ProvenTransaction, Transaction, TxWithFee}

trait InvokeTransaction extends Transaction with InvokeScriptTransactionLike with ProvenTransaction with TxWithFee.InCustomAsset with FastHashId {
  override val checkedAssets: Seq[Asset.IssuedAsset] = super[InvokeScriptTransactionLike].checkedAssets
}

object InvokeTransaction {
  val DefaultCall: FUNCTION_CALL = FUNCTION_CALL(User(ContractEvaluator.DEFAULT_FUNC_NAME), Nil)
}

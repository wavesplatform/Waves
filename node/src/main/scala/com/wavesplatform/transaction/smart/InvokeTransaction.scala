package com.wavesplatform.transaction.smart
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator
import com.wavesplatform.state.diffs.invoke.InvokeScriptLike
import com.wavesplatform.transaction.{Asset, FastHashId, ProvenTransaction, TxWithFee, VersionedTransaction}

trait InvokeTransaction extends InvokeScriptLike with ProvenTransaction with TxWithFee.InCustomAsset with FastHashId with VersionedTransaction {
  override def checkedAssets: Seq[Asset.IssuedAsset] = super[InvokeScriptLike].checkedAssets
}

object InvokeTransaction {
  val defaultCall: FUNCTION_CALL = FUNCTION_CALL(User(ContractEvaluator.DEFAULT_FUNC_NAME), Nil)
}

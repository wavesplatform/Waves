package com.wavesplatform.state.diffs

import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.ProvenTransaction

private[diffs] object DiffsCommon {
  def verifierComplexity(script: Script): Long = script match {
    case ContractScriptImpl(_, DApp(_, _, _, Some(vf)), cm) if cm.contains(vf.u.name) => cm(vf.u.name)
    case _ => script.complexity
  }

  def functionComplexity(script: Script, maybeCall: Option[FUNCTION_CALL]): Long = maybeCall match {
    case Some(call) =>
      script.complexityMap.getOrElse(call.function.funcName, script.complexity)

    case None =>
      script.expr match {
        case DApp(_, _, Some(df), _) if script.complexityMap.contains(df.u.name) => script.complexityMap(df.u.name)
        case _ => script.complexity
      }
  }

  def countScriptRuns(blockchain: Blockchain, tx: ProvenTransaction): Int =
    tx.checkedAssets().count(blockchain.hasAssetScript) + Some(tx.sender.toAddress).count(blockchain.hasScript)

  def countScriptsComplexity(blockchain: Blockchain, tx: ProvenTransaction): Long = {
    val assetsComplexity = tx
      .checkedAssets()
      .flatMap(blockchain.assetDescription)
      .flatMap(_.script)
      .map(verifierComplexity)
      .sum

    val accountComplexity = blockchain
      .accountScript(tx.sender.toAddress)
      .fold(0L)(verifierComplexity)

    assetsComplexity + accountComplexity
  }
}

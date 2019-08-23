package com.wavesplatform.state.diffs

import com.wavesplatform.features.EstimatorProvider._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.DEFAULT_FUNC_NAME
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.ProvenTransaction
import cats.implicits._
import com.wavesplatform.lang.v1.estimator.ScriptEstimator

private[diffs] object DiffsCommon {
  def verifierComplexity(script: Script, estimator: ScriptEstimator): Either[String, Long] =
    Script.complexityInfo(script, estimator)
      .map(calcVerifierComplexity(script, _))

  private def calcVerifierComplexity(
    script:     Script,
    complexity: (Long, Map[String, Long])
  ): Long = {
    val (totalComplexity, cm) = complexity
    script match {
      case ContractScriptImpl(_, DApp(_, _, _, Some(vf))) if cm.contains(vf.u.name) => cm(vf.u.name)
      case _ => totalComplexity
    }
  }

  def functionComplexity(
    script:    Script,
    estimator: ScriptEstimator,
    maybeCall: Option[FUNCTION_CALL]
  ): Either[String, Long] =
    Script.complexityInfo(script, estimator)
      .map(calcFunctionComplexity(script, maybeCall, _))

  private def calcFunctionComplexity(
    script:     Script,
    maybeCall:  Option[FUNCTION_CALL],
    complexity: (Long, Map[String, Long])
  ): Long = {
    val (totalComplexity, cm) = complexity
    maybeCall match {
      case Some(call) =>
        cm.getOrElse(call.function.funcName, totalComplexity)

      case None =>
        script.expr match {
          case DApp(_, _, callables, _) =>
            callables
              .find(f => (f.u.name == DEFAULT_FUNC_NAME) && f.u.args.isEmpty)
              .flatMap(f => cm.get(f.u.name))
              .getOrElse(totalComplexity)

          case _ => totalComplexity
        }
    }
  }

  def countScriptRuns(blockchain: Blockchain, tx: ProvenTransaction): Int =
    tx.checkedAssets().count(blockchain.hasAssetScript) + Some(tx.sender.toAddress).count(blockchain.hasScript)

  def countScriptsComplexity(blockchain: Blockchain, tx: ProvenTransaction): Either[String, Long] =
    for {
      assetsComplexity <- tx
        .checkedAssets()
        .toList
        .flatMap(blockchain.assetDescription)
        .flatMap(_.script)
        .traverse(verifierComplexity(_, blockchain.estimator))

      accountComplexity <- blockchain
        .accountScript(tx.sender.toAddress)
        .traverse(verifierComplexity(_, blockchain.estimator))

    } yield assetsComplexity.sum + accountComplexity.getOrElse(0L)
}

package com.wavesplatform.state.diffs

import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import com.wavesplatform.features.ComplexityCheckPolicyProvider._
import com.wavesplatform.features.EstimatorProvider._
import com.wavesplatform.features.BlockchainFeatures.RideV6
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.state.{AccountScriptInfo, Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.SetScriptTransaction

object SetScriptTransactionDiff {
  def apply(blockchain: Blockchain)(tx: SetScriptTransaction): Either[ValidationError, Diff] =
    for {
      callableComplexities <- tx.script match {
        case Some(ContractScriptImpl(version, dApp)) => estimate(blockchain, version, dApp, checkOverflow = blockchain.checkEstimatorSumOverflow)
        case _                                       => Right(Map[Int, Map[String, Long]]())
      }
      verifierWithComplexity <- DiffsCommon.countVerifierComplexity(tx.script, blockchain, isAsset = false)
      scriptWithComplexities <- verifierWithComplexity
        .map {
          case (script, verifierComplexity) =>
            AccountScriptInfo(tx.sender, script, verifierComplexity, callableComplexities)
        }
        .traverseTap(checkOverflow(blockchain, _))
    } yield
      Diff(
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
        scripts = Map(tx.sender.toAddress    -> scriptWithComplexities),
        scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)
      )

  def estimate(
      blockchain: Blockchain,
      version: StdLibVersion,
      dApp: DApp,
      checkOverflow: Boolean
  ): Either[GenericError, Map[Int, Map[String, Long]]] = {
    val callables = dApp.copy(verifierFuncOpt = None)
    val actualComplexities =
      for {
        currentComplexity <- ContractScript.estimateComplexity(
          version,
          callables,
          blockchain.estimator,
          fixEstimateOfVerifier = blockchain.isFeatureActivated(RideV6),
          useReducedVerifierLimit = blockchain.useReducedVerifierComplexityLimit
        )
        nextComplexities <- estimateNext(blockchain, version, callables)
        complexitiesByEstimator = (currentComplexity :: nextComplexities).mapWithIndex {
          case ((_, complexitiesByCallable), i) => (i + blockchain.estimator.version, complexitiesByCallable)
        }.toMap
      } yield complexitiesByEstimator

    actualComplexities.leftMap(GenericError(_))
  }

  private def estimateNext(
      blockchain: Blockchain,
      version: StdLibVersion,
      dApp: DApp
  ): Either[String, List[(Long, Map[String, Long])]] =
    ScriptEstimator.all(fixOverflow = blockchain.checkEstimationOverflow)
      .drop(blockchain.estimator.version)
      .traverse(
        se =>
          ContractScript
            .estimateComplexityExact(version, dApp, se, fixEstimateOfVerifier = blockchain.isFeatureActivated(RideV6))
            .map { case ((_, maxComplexity), complexities) => (maxComplexity, complexities) })

  private def checkOverflow(blockchain: Blockchain, s: AccountScriptInfo): Either[GenericError, Unit] =
    if (blockchain.checkEstimationOverflow)
      if (s.verifierComplexity < 0)
        Left(GenericError("Unexpected negative verifier complexity"))
      else
        s.complexitiesByEstimator
          .values
          .flatten
          .collectFirst { case (name, complexity) if complexity < 0 => GenericError(s"Unexpected negative callable `$name` complexity") }
          .toLeft(())
    else
      Right(())
}

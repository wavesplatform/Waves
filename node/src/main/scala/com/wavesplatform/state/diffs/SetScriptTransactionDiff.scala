package com.wavesplatform.state.diffs

import cats.instances.list.*
import cats.instances.option.*
import cats.syntax.either.*
import cats.syntax.traverse.*
import com.google.common.cache.{Cache, CacheBuilder}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.RideV6
import com.wavesplatform.features.ComplexityCheckPolicyProvider.*
import com.wavesplatform.features.EstimatorProvider.*
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.ContractLimits.{MaxContractSizeInBytes, MaxContractSizeInBytesV6, MaxExprSizeInBytes}
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.state.{AccountScriptInfo, Blockchain, Portfolio, StateSnapshot}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.SetScriptTransaction

object SetScriptTransactionDiff {
  def apply(blockchain: Blockchain)(tx: SetScriptTransaction): Either[ValidationError, StateSnapshot] =
    for {
      // Validate script size limit
      _ <- tx.script match {
        case Some(script) =>
          if (script.isInstanceOf[ExprScript]) scriptSizeValidation(script, MaxExprSizeInBytes)
          else if (blockchain.isFeatureActivated(BlockchainFeatures.RideV6)) scriptSizeValidation(script, MaxContractSizeInBytesV6)
          else scriptSizeValidation(script, MaxContractSizeInBytes)

        case None => Right(())
      }

      scriptWithComplexities <- tx.script.traverse { script =>
        complexities(blockchain, script).map { case (callableComplexities, verifierComplexity) =>
          AccountScriptInfo(tx.sender, script, verifierComplexity, callableComplexities)
        }
      }
      _ <- scriptWithComplexities.traverse(checkOverflow(blockchain, _))
      snapshot <- StateSnapshot.build(
        blockchain,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee.value)),
        accountScripts = Map(tx.sender -> scriptWithComplexities)
      )
    } yield snapshot

  // Estimation depends only on the script and these blockchain settings, and estimating a dApp against every estimator is costly: past testnet
  // height 2.1M, two thirds of SetScript transactions set a script that was set before.
  private final case class EstimationKey(
      script: ByteStr,
      estimator: ScriptEstimator,
      rideV6: Boolean,
      reducedVerifierLimit: Boolean,
      checkEstimationOverflow: Boolean,
      v1PreCheck: Boolean
  )
  private val estimations: Cache[EstimationKey, Either[ValidationError, (Map[Int, Map[String, Long]], Long)]] =
    CacheBuilder.newBuilder().maximumSize(1000).build()

  // Callable complexities by estimator version, and the verifier complexity. Not computed through the cache's loader, so an exception
  // thrown by an estimator reaches the caller as it is.
  private def complexities(blockchain: Blockchain, script: Script): Either[ValidationError, (Map[Int, Map[String, Long]], Long)] = {
    val key = EstimationKey(
      script.bytes(),
      blockchain.estimator,
      blockchain.isFeatureActivated(RideV6),
      blockchain.useReducedVerifierComplexityLimit,
      blockchain.checkEstimationOverflow,
      DiffsCommon.useV1PreCheck(blockchain)
    )
    Option(estimations.getIfPresent(key)).getOrElse {
      val result = for {
        callableComplexities <- script match {
          case ContractScriptImpl(version, dApp) => estimate(blockchain, version, dApp)
          case _                                 => Right(Map[Int, Map[String, Long]]())
        }
        verifierWithComplexity <- DiffsCommon.countVerifierComplexity(Some(script), blockchain, isAsset = false)
      } yield (callableComplexities, verifierWithComplexity.fold(0L)(_._2))
      estimations.put(key, result)
      result
    }
  }

  def scriptSizeValidation(value: Script, limit: Int = MaxExprSizeInBytes): Either[GenericError, Unit] = {
    Either.cond(
      value.bytes().size <= limit,
      (),
      GenericError(s"Script is too large: ${value.bytes().size} bytes > $limit bytes")
    )
  }

  def estimate(
      blockchain: Blockchain,
      version: StdLibVersion,
      dApp: DApp
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
        complexitiesByEstimator = (currentComplexity :: nextComplexities).mapWithIndex { case ((_, complexitiesByCallable), i) =>
          (i + blockchain.estimator.version, complexitiesByCallable)
        }.toMap
      } yield complexitiesByEstimator

    actualComplexities.leftMap(GenericError(_))
  }

  private def estimateNext(
      blockchain: Blockchain,
      version: StdLibVersion,
      dApp: DApp
  ): Either[String, List[(Long, Map[String, Long])]] =
    ScriptEstimator
      .all(fixOverflow = blockchain.checkEstimationOverflow)
      .drop(blockchain.estimator.version)
      .traverse(se =>
        ContractScript
          .estimateComplexityExact(version, dApp, se, fixEstimateOfVerifier = blockchain.isFeatureActivated(RideV6))
          .map { case ((_, maxComplexity), complexities) => (maxComplexity, complexities) }
      )

  private def checkOverflow(blockchain: Blockchain, s: AccountScriptInfo): Either[GenericError, Unit] =
    if (blockchain.checkEstimationOverflow)
      if (s.verifierComplexity < 0)
        Left(GenericError("Unexpected negative verifier complexity"))
      else
        s.complexitiesByEstimator.values.flatten
          .collectFirst { case (name, complexity) if complexity < 0 => GenericError(s"Unexpected negative callable `$name` complexity") }
          .toLeft(())
    else
      Right(())
}

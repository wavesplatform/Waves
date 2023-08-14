package com.wavesplatform.ride

import com.wavesplatform.features.{BlockchainFeatures, ComplexityCheckPolicyProvider}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.Script.ComplexityInfo
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.ride.runner.caches.ActivatedFeatures
import com.wavesplatform.state.Blockchain

package object runner {

  // See DiffCommon.countVerifierComplexity
  def estimate(
      height: Int,
      activatedFeatures: ActivatedFeatures,
      estimator: ScriptEstimator,
      script: Script,
      isAsset: Boolean,
      withCombinedContext: Boolean = false
  ): ComplexityInfo = {
    val fixEstimateOfVerifier    = Blockchain.isFeatureActivated(activatedFeatures, BlockchainFeatures.RideV6, height)
    val useContractVerifierLimit = !isAsset && ComplexityCheckPolicyProvider.useReducedVerifierComplexityLimit(activatedFeatures)

    Script.complexityInfo(script, estimator, fixEstimateOfVerifier, useContractVerifierLimit, withCombinedContext = withCombinedContext) match {
      case Right(x) => x
      case Left(e)  => throw new RuntimeException(s"Can't get a complexity info of script: $e")
    }
  }

}

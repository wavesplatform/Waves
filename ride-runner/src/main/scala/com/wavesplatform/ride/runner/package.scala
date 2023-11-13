package com.wavesplatform.ride

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.ComplexityCheckPolicyProvider.VerifierComplexityCheckExt
import com.wavesplatform.features.EstimatorProvider.EstimatorBlockchainExt
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.Script.ComplexityInfo
import com.wavesplatform.state.Blockchain

package object runner {

  // See DiffCommon.countVerifierComplexity
  def estimate(
      blockchain: Blockchain,
      script: Script,
      isAsset: Boolean,
      withCombinedContext: Boolean = false
  ): ComplexityInfo = {
    val fixEstimateOfVerifier    = blockchain.isFeatureActivated(BlockchainFeatures.RideV6)
    val useContractVerifierLimit = !isAsset && blockchain.useReducedVerifierComplexityLimit

    Script.complexityInfo(
      script,
      blockchain.estimator,
      fixEstimateOfVerifier,
      useContractVerifierLimit,
      withCombinedContext = withCombinedContext
    ) match {
      case Right(x) => x
      case Left(e)  => throw new RuntimeException(s"Can't get a complexity info of script: $e")
    }
  }

}

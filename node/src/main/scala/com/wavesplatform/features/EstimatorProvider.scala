package com.wavesplatform.features

import com.wavesplatform.state.Blockchain
import BlockchainFeatures.ReduceNFTFee
import FeatureProvider._
import com.wavesplatform.lang.v1.estimator.{ScriptEstimator, ScriptEstimatorV1}
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2

object EstimatorProvider {
  implicit class EstimatorBlockchainExt(b: Blockchain) {
    def estimator(): ScriptEstimator =
      if (b.isFeatureActivated(ReduceNFTFee)) ScriptEstimatorV2 else ScriptEstimatorV1
  }
}

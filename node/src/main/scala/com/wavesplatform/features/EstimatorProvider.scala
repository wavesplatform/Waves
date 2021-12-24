package com.wavesplatform.features

import com.wavesplatform.features.BlockchainFeatures.*
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.estimator.{ScriptEstimator, ScriptEstimatorV1}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.Blockchain

object EstimatorProvider {

  implicit class EstimatorBlockchainExt(b: Blockchain) {
    def estimator: ScriptEstimator =
      if (b.isFeatureActivated(BlockV5))
        ScriptEstimatorV3(
          fixOverflow = checkEstimatorSumOverflow,
          overhead = !b.isFeatureActivated(RideV6)
        )
      else if (b.isFeatureActivated(BlockReward)) ScriptEstimatorV2
      else ScriptEstimatorV1

    def storeEvaluatedComplexity: Boolean =
      b.isFeatureActivated(SynchronousCalls)

    def checkEstimationOverflow: Boolean =
      b.height >= b.settings.functionalitySettings.estimationOverflowFixHeight

    def checkEstimatorSumOverflow: Boolean =
      b.height >= b.settings.functionalitySettings.estimatorSumOverflowFixHeight
  }

  implicit class EstimatorWavesSettingsExt(ws: WavesSettings) {
    def estimator: ScriptEstimator =
      if (ws.featuresSettings.supported.contains(RideV6.id)) ScriptEstimatorV3(fixOverflow = true, overhead = false)
      else if (ws.featuresSettings.supported.contains(BlockV5.id)) ScriptEstimatorV3(fixOverflow = true, overhead = true)
      else if (ws.featuresSettings.supported.contains(BlockReward.id)) ScriptEstimatorV2
      else ScriptEstimatorV1
  }
}

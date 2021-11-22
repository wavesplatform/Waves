package com.wavesplatform.features

import com.wavesplatform.features.BlockchainFeatures._
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.estimator.{ScriptEstimator, ScriptEstimatorV1}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.Blockchain

object EstimatorProvider {
  private val estimatorV3NoOverhead   = ScriptEstimatorV3(overhead = false)
  private val estimatorV3WithOverhead = ScriptEstimatorV3(overhead = true)

  implicit class EstimatorBlockchainExt(b: Blockchain) {
    def estimator: ScriptEstimator =
      if (b.isFeatureActivated(RideV6)) estimatorV3NoOverhead
      else if (b.isFeatureActivated(BlockV5)) estimatorV3WithOverhead
      else if (b.isFeatureActivated(BlockReward)) ScriptEstimatorV2
      else ScriptEstimatorV1

    def storeEvaluatedComplexity: Boolean =
      b.isFeatureActivated(SynchronousCalls)

    def complexityOverhead: Boolean =
      !b.isFeatureActivated(RideV6)
  }

  implicit class EstimatorWavesSettingsExt(ws: WavesSettings) {
    def estimator: ScriptEstimator =
      if (ws.featuresSettings.supported.contains(RideV6.id)) estimatorV3NoOverhead
      else if (ws.featuresSettings.supported.contains(BlockV5.id)) estimatorV3WithOverhead
      else if (ws.featuresSettings.supported.contains(BlockReward.id)) ScriptEstimatorV2
      else ScriptEstimatorV1
  }
}

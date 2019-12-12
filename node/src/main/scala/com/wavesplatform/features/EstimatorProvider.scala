package com.wavesplatform.features

import com.wavesplatform.features.BlockchainFeatures.{BlockReward, MultiPaymentInvokeScript}
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.estimator.{ScriptEstimator, ScriptEstimatorV1}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.Blockchain

object EstimatorProvider {
  implicit class EstimatorBlockchainExt(b: Blockchain) {
    val estimator: ScriptEstimator =
      if (b.isFeatureActivated(MultiPaymentInvokeScript)) ScriptEstimatorV3
      else if (b.isFeatureActivated(BlockReward)) ScriptEstimatorV2
      else ScriptEstimatorV1

    val useStoredCallableComplexities: Boolean =
      b.isFeatureActivated(MultiPaymentInvokeScript)
  }

  implicit class EstimatorWavesSettingsExt(ws: WavesSettings) {
    val estimator: ScriptEstimator =
      if (ws.featuresSettings.supported.contains(MultiPaymentInvokeScript.id)) ScriptEstimatorV3
      else if (ws.featuresSettings.supported.contains(BlockReward.id)) ScriptEstimatorV2
      else ScriptEstimatorV1
  }
}

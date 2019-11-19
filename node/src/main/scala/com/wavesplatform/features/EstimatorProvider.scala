package com.wavesplatform.features

import com.wavesplatform.features.BlockchainFeatures.{BlockReward, MultiPaymentInvokeScript}
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.estimator.{ScriptEstimator, ScriptEstimatorV1}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.DiffsCommon

object EstimatorProvider {
  implicit class EstimatorBlockchainExt(b: Blockchain) {
    val estimator: ScriptEstimator =
      if (b.isFeatureActivated(MultiPaymentInvokeScript)) ScriptEstimatorV3
      else if (b.isFeatureActivated(BlockReward)) ScriptEstimatorV2
      else ScriptEstimatorV1

    def invocationComplexity(script: Script, call: Option[FUNCTION_CALL]): Either[String, Long] =
      if (b.isFeatureActivated(MultiPaymentInvokeScript))
        DiffsCommon.limitFreeComplexity(script, estimator, call)
      else
        DiffsCommon.functionComplexity(script, estimator, call)

    def cachedVerifierComplexity(script: Script): Either[String, Long] =
      if (b.isFeatureActivated(MultiPaymentInvokeScript))
        Script.limitFreeVerifierComplexity(script, estimator)
      else
        Script.verifierComplexity(script, estimator)
  }

  implicit class EstimatorWavesSettingsExt(ws: WavesSettings) {
    val estimator: ScriptEstimator =
      if (ws.featuresSettings.supported.contains(MultiPaymentInvokeScript.id)) ScriptEstimatorV3
      else if (ws.featuresSettings.supported.contains(BlockReward.id)) ScriptEstimatorV2
      else ScriptEstimatorV1
  }
}

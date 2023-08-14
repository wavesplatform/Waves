package com.wavesplatform.ride.runner

import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.{ScriptEstimator, ScriptEstimatorV1}
import com.wavesplatform.ride.runner.caches.ActivatedFeatures
import com.wavesplatform.state.AccountScriptInfo

package object blockchain {
  def mkAccountScript(
      estimator: ScriptEstimator,
      activatedFeatures: ActivatedFeatures,
      height: Int,
      publicKey: PublicKey,
      script: Script
  ): AccountScriptInfo = {
    val complexityInfo = Set(ScriptEstimatorV1, ScriptEstimatorV2, estimator).map { estimator =>
      estimator.version -> estimate(height, activatedFeatures, estimator, script, isAsset = false, withCombinedContext = true)
    }

    val (lastEstimatorVersion, lastComplexityInfo) = complexityInfo.last
    AccountScriptInfo(
      script = script,
      publicKey = publicKey,
      verifierComplexity = lastComplexityInfo.verifierComplexity,
      complexitiesByEstimator = complexityInfo
        .map { case (v, complexityInfo) => v -> complexityInfo.callableComplexities }
        .toMap
        .updated(lastEstimatorVersion, lastComplexityInfo.callableComplexities) // to preserve
    )
  }
}

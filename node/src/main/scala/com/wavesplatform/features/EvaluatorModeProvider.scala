package com.wavesplatform.features

import com.wavesplatform.state.Blockchain

object EvaluatorModeProvider {
  implicit class EvaluatorModeExt(b: Blockchain) {
    def newEvaluatorMode: Boolean =
      b.isFeatureActivated(BlockchainFeatures.RideV6)
  }
}

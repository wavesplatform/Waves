package com.wavesplatform.features

import com.wavesplatform.state.Blockchain

object EvaluatorFixProvider {
  implicit class newModeExt(b: Blockchain) {
    def newMode: Boolean =
      b.isFeatureActivated(BlockchainFeatures.RideV6)
  }
}

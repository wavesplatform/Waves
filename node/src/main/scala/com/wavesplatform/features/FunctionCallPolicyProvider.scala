package com.wavesplatform.features

import com.wavesplatform.state.Blockchain

object FunctionCallPolicyProvider {
  implicit class MultiPaymentAllowedExt(b: Blockchain) {
    def callableListArgumentsAllowed: Boolean =
      b.activatedFeatures.contains(BlockchainFeatures.BlockV5.id)

    def checkSyncCallArgTypes: Boolean =
      b.isFeatureActivated(BlockchainFeatures.RideV6)
  }
}

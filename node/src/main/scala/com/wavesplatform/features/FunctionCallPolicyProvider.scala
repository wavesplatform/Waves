package com.wavesplatform.features

import com.wavesplatform.state.Blockchain

object FunctionCallPolicyProvider {
  implicit class MultiPaymentAllowedExt(b: Blockchain) {
    val callableListArgumentsAllowed: Boolean =
      b.activatedFeatures.contains(BlockchainFeatures.BlockV5.id)
  }
}

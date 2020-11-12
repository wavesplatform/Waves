package com.wavesplatform.features

import com.wavesplatform.state.Blockchain

object MultiPaymentPolicyProvider {
  implicit class MultiPaymentAllowedExt(b: Blockchain) {
    def allowsMultiPayment: Boolean =
      b.activatedFeatures.contains(BlockchainFeatures.BlockV5.id)
  }
}

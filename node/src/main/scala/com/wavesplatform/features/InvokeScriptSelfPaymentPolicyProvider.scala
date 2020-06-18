package com.wavesplatform.features

import com.wavesplatform.state.Blockchain

object InvokeScriptSelfPaymentPolicyProvider {
  implicit class SelfPaymentPolicyBlockchainExt(b: Blockchain) {
    val disallowSelfPayment: Boolean =
      b.isFeatureActivated(BlockchainFeatures.BlockV5)
  }
}

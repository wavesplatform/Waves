package com.wavesplatform.features

import com.wavesplatform.state.Blockchain

object MultiPaymentPolicyProvider {
  implicit class MultiPaymentAllowedExt(b: Blockchain) {
    val multiPaymentAllowed: Boolean =
      b.activatedFeatures.contains(BlockchainFeatures.MultiPaymentInvokeScript.id)
  }
}

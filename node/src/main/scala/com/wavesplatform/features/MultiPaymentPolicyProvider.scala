package com.wavesplatform.features

import com.wavesplatform.state.Blockchain

object MultiPaymentPolicyProvider {
  implicit class MultiPaymentAllowedExt(b: Blockchain) {
    val allowsMultiPayment: Boolean =
      b.activatedFeatures.contains(BlockchainFeatures.MultiPaymentInvokeScript.id)
  }
}

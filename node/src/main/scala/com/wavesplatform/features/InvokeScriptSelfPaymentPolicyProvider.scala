package com.wavesplatform.features

import com.wavesplatform.state.Blockchain
import com.wavesplatform.features.FeatureProvider._

object InvokeScriptSelfPaymentPolicyProvider {
  implicit class SelfPaymentPolicyBlockchainExt(b: Blockchain) {
    val disallowSelfPayment: Boolean =
      b.isFeatureActivated(BlockchainFeatures.MultiPaymentInvokeScript)
  }
}

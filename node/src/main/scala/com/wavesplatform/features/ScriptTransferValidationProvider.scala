package com.wavesplatform.features

import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.state.Blockchain

object ScriptTransferValidationProvider {
  implicit class PassCorrectAssetIdExt(b: Blockchain) {
    def passCorrectAssetId: Boolean =
      b.isFeatureActivated(BlockchainFeatures.MultiPaymentInvokeScript)
  }
}

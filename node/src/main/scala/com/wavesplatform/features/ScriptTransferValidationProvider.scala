package com.wavesplatform.features

import com.wavesplatform.state.Blockchain

object ScriptTransferValidationProvider {
  implicit class PassCorrectAssetIdExt(b: Blockchain) {
    def passCorrectAssetId: Boolean =
      b.isFeatureActivated(BlockchainFeatures.BlockV5)
  }
}

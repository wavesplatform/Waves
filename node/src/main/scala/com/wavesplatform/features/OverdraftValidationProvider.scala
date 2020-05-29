package com.wavesplatform.features

import com.wavesplatform.state.Blockchain

object OverdraftValidationProvider {
  implicit class ConsistentOverdraftExt(b: Blockchain) {
    val useCorrectPaymentCheck: Boolean =
      b.activatedFeatures.contains(BlockchainFeatures.BlockV5.id)
  }
}

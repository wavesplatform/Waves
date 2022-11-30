package com.wavesplatform.features

import com.wavesplatform.state.Blockchain

object ComplexityCheckPolicyProvider {
  implicit class VerifierComplexityCheckExt(b: Blockchain) {
    def useReducedVerifierComplexityLimit: Boolean = ComplexityCheckPolicyProvider.useReducedVerifierComplexityLimit(b.activatedFeatures)
  }

  def useReducedVerifierComplexityLimit(activatedFeatures: Map[Short, Int]): Boolean =
    activatedFeatures.contains(BlockchainFeatures.BlockV5.id)
}

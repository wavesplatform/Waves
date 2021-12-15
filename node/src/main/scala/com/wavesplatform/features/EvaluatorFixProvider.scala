package com.wavesplatform.features

import com.wavesplatform.state.Blockchain

object EvaluatorFixProvider {
  implicit class CorrectFunctionCallScopeExt(b: Blockchain) {
    def correctFunctionCallScope: Boolean =
      b.height >= b.settings.functionalitySettings.estimatorSumOverflowFixHeight
  }
}

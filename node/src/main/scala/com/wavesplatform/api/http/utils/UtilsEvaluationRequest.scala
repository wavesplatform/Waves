package com.wavesplatform.api.http.utils

import com.wavesplatform.state.BlockchainOverrides

trait UtilsEvaluationRequest {
  def state: Option[BlockchainOverrides]
}

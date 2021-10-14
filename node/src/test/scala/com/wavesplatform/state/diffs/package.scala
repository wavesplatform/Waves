package com.wavesplatform.state

package object diffs {
  val ENOUGH_AMT: Long = Long.MaxValue / 3

  def produceRejectOrFailedDiff(errorMessage: String, requireFailed: Boolean = false): DiffProduceError = new DiffProduceError(errorMessage, requireFailed)
}

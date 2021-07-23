package com.wavesplatform.state

package object diffs {
  val ENOUGH_AMT: Long = Long.MaxValue / 3

  def produceE(errorMessage: String, requireFailed: Boolean): DiffProduceError = new DiffProduceError(errorMessage, requireFailed)
}

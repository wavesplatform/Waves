package com.wavesplatform.state

import org.scalatest.Matchers

package object diffs extends Matchers {
  val ENOUGH_AMT: Long = Long.MaxValue / 3

  def produce(errorMessage: String, requireFailed: Boolean = false): DiffProduceError = new DiffProduceError(errorMessage, requireFailed)
}

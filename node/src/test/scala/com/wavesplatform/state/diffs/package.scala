package com.wavesplatform.state

import com.wavesplatform.common.state.diffs.ProduceError
import org.scalatest.Matchers

package object diffs extends Matchers {
  val ENOUGH_AMT: Long = Long.MaxValue / 3

  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)
}

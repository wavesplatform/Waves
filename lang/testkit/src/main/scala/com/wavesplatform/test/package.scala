package com.wavesplatform

import org.scalatest.matchers.Matcher

package object test {
  implicit class NumericExt[N](val x: N) extends AnyVal {
    def waves(implicit n: Numeric[N]): Long = (BigDecimal(n.toDouble(x)) * 1_0000_0000L).toLong
  }

  def produce(err: String): Matcher[Either[_, _]] = new ProduceError(err)
}

package com.wavesplatform.matcher

import scala.math.BigDecimal.RoundingMode

package object util {

  def multiplyLongByDouble(l: Long, d: Double): Long = (BigDecimal(l) * d).setScale(0, RoundingMode.HALF_UP).toLong

}

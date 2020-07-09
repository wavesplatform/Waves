package com.wavesplatform.it

import com.wavesplatform.settings.Constants
import com.wavesplatform.state.DataEntry

package object util {
  implicit class DoubleExt(val d: Double) extends AnyVal {
    def waves: Long = (BigDecimal(d) * Constants.UnitsInWave).toLong
  }
  implicit class TypedDataEntry(entry: DataEntry[_]) {
    def as[T]: T = entry.asInstanceOf[T]
  }
}

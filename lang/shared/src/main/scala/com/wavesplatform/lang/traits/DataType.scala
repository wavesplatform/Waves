package com.wavesplatform.lang.traits

import com.wavesplatform.lang.Terms.{BOOLEAN, BYTEVECTOR, LONG, TYPE}

sealed abstract case class DataType(innerType: TYPE)
object DataType {
  object Boolean   extends DataType(BOOLEAN)
  object Long      extends DataType(LONG)
  object ByteArray extends DataType(BYTEVECTOR)
}

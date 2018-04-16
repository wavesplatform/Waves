package com.wavesplatform.lang.v1.traits

import com.wavesplatform.lang.v1.Terms.{BOOLEAN, BYTEVECTOR, LONG, TYPE}

sealed abstract case class DataType(innerType: TYPE)
object DataType {
  object Boolean   extends DataType(BOOLEAN)
  object Long      extends DataType(LONG)
  object ByteArray extends DataType(BYTEVECTOR)
}

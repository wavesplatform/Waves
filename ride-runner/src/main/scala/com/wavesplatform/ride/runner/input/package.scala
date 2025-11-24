package com.wavesplatform.ride.runner

package object input {
  opaque type StringOrBytesAsByteArray = Array[Byte]
  object StringOrBytesAsByteArray {
    def apply(bs: Array[Byte]): StringOrBytesAsByteArray         = bs
    extension (s: StringOrBytesAsByteArray) def arr: Array[Byte] = s
  }
}

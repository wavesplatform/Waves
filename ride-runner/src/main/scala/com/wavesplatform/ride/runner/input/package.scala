package com.wavesplatform.ride.runner

import supertagged.TaggedType

package object input {
  object StringOrBytesAsByteArray extends TaggedType[Array[Byte]]
  type StringOrBytesAsByteArray = StringOrBytesAsByteArray.Type
}

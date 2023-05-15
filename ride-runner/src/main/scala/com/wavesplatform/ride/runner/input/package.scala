package com.wavesplatform.ride.runner

import com.google.protobuf.ByteString
import com.wavesplatform.common.state.ByteStr
import supertagged.TaggedType

package object input {
  object StringOrBytesAsByteStr extends TaggedType[ByteStr]
  type StringOrBytesAsByteStr = StringOrBytesAsByteStr.Type

  object StringOrBytesAsByteString extends TaggedType[ByteString]
  type StringOrBytesAsByteString = StringOrBytesAsByteString.Type
}

package com.wavesplatform.ride.runner

import com.wavesplatform.common.state.ByteStr
import supertagged.TaggedType

package object input {
  object StringOrBytes extends TaggedType[ByteStr]
  type StringOrBytes = StringOrBytes.Type
}

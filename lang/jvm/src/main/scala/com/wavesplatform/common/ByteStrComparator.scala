package com.wavesplatform.common

import com.google.common.primitives.UnsignedBytes
import com.wavesplatform.common.state.ByteStr

object ByteStrComparator {
  def compare(bs1: ByteStr, bs2: ByteStr): Int = UnsignedBytes.lexicographicalComparator().compare(bs1.arr, bs2.arr)
}

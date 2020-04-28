package com.wavesplatform.common

import com.wavesplatform.common.state.ByteStr

object ByteStrComparator {
  def compare(bs1: ByteStr, bs2: ByteStr): Int = {
    val end1: Int = if (bs1.arr.length < bs2.arr.length) bs1.arr.length else bs2.arr.length
    var i: Int    = 0
    while (i < end1) {
      val a: Int = bs1.arr(i) & 0xff
      val b: Int = bs2.arr(i) & 0xff
      if (a != b) {
        return a - b
      }
      i = i + 1
    }
    bs1.arr.length - bs2.arr.length
  }
}

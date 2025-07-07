package com.wavesplatform.finalization

import com.wavesplatform.common.state.ByteStr

opaque type BlsPublicKey = ByteStr

object BlsPublicKey {
  def apply(x: ByteStr): BlsPublicKey     = x
  def apply(x: Array[Byte]): BlsPublicKey = ByteStr(x)

  extension (x: BlsPublicKey) {
    def toByteStr: ByteStr = x
  }
}

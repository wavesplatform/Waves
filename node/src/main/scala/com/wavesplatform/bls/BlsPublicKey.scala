package com.wavesplatform.bls

import com.wavesplatform.common.state.ByteStr

opaque type BlsPublicKey = ByteStr

object BlsPublicKey {
  def apply(x: ByteStr): BlsPublicKey     = x
  def apply(x: Array[Byte]): BlsPublicKey = ByteStr(x)

  extension (self: BlsPublicKey) {
    def asByteStr: ByteStr = self
  }
}

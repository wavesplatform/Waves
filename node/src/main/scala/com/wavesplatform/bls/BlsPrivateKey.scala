package com.wavesplatform.bls

import com.wavesplatform.common.state.ByteStr
import supranational.blst.SecretKey as BlstSecretKey
import supranational.blst.{P1, SecretKey as BlstSecretKey}

opaque type BlsPrivateKey = BlstSecretKey

object BlsPrivateKey {
  def apply(x: BlstSecretKey): BlsPrivateKey = x

  extension (self: BlsPrivateKey) {
    def createPublicKey(): BlsPublicKey = {
      val pk = new P1(self)
      BlsPublicKey(ByteStr(pk.serialize()))
    }
  }
}

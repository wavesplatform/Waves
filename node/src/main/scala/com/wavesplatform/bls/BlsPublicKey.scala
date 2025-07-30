package com.wavesplatform.bls

import com.wavesplatform.common.state.ByteStr
import supranational.blst

opaque type BlsPublicKey = ByteStr

object BlsPublicKey {
  val SizeInBytes = 48 // TODO: ???

  def apply(x: ByteStr): BlsPublicKey     = x
  def apply(x: Array[Byte]): BlsPublicKey = ByteStr(x)

  extension (self: BlsPublicKey) {
    def asByteStr: ByteStr = self

    def verify(message: Array[Byte], signature: Array[Byte]): Boolean = {
      val _sig = new blst.P2_Affine(signature)
      val _pk  = new blst.P1_Affine(self.arr)
      if (!_pk.in_group()) throw new java.lang.RuntimeException("disaster") // TODO:

      val ctx = new blst.Pairing(true, BlsDomainSeparationTag)
      ctx.aggregate(_pk, _sig, message, self.arr)
      ctx.commit()
      ctx.finalverify()
    }
  }
}

package com.wavesplatform.common.crypto

import java.security.spec.X509EncodedKeySpec
import java.security.{KeyFactory, Signature}

import com.wavesplatform.common.state.ByteStr

object RSA {

  def verify(message: ByteStr, signature: Array[Byte], publicKey: Array[Byte]): Boolean = {
    val publicSignature = Signature.getInstance("SHA256withRSA")

    val jPublicKeySpec = new X509EncodedKeySpec(publicKey)
    val jPublicKey     = KeyFactory.getInstance("RSA").generatePublic(jPublicKeySpec)

    publicSignature.initVerify(jPublicKey)
    publicSignature.update(message.arr)

    publicSignature.verify(signature)
  }
}

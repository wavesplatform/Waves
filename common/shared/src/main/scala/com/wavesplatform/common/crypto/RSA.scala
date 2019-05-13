package com.wavesplatform.common.crypto

import java.security.spec.X509EncodedKeySpec
import java.security.{KeyFactory, Signature}

import com.wavesplatform.common.state.ByteStr

object RSA {

  sealed trait DigestAlgorithm
  case object SHA1     extends DigestAlgorithm
  case object SHA224   extends DigestAlgorithm
  case object SHA256   extends DigestAlgorithm
  case object SHA384   extends DigestAlgorithm
  case object SHA512   extends DigestAlgorithm
  case object SHA3_224 extends DigestAlgorithm
  case object SHA3_256 extends DigestAlgorithm
  case object SHA3_384 extends DigestAlgorithm
  case object SHA3_512 extends DigestAlgorithm

  def verify(digestAlgorithm: DigestAlgorithm, message: ByteStr, signature: Array[Byte], publicKey: Array[Byte]): Boolean = {
    val algorithm = {
      val digestPrefix = digestAlgorithm match {
        case SHA1     => "SHA1"
        case SHA224   => "SHA224"
        case SHA256   => "SHA256"
        case SHA384   => "SHA384"
        case SHA512   => "SHA512"
        case SHA3_224 => "SHA3-224"
        case SHA3_256 => "SHA3-256"
        case SHA3_384 => "SHA3-384"
        case SHA3_512 => "SHA3-512"
      }

      s"${digestPrefix}withRSA"
    }

    val publicSignature = Signature.getInstance(algorithm)

    val jPublicKeySpec = new X509EncodedKeySpec(publicKey)
    val jPublicKey     = KeyFactory.getInstance("RSA").generatePublic(jPublicKeySpec)

    publicSignature.initVerify(jPublicKey)
    publicSignature.update(message.arr)

    publicSignature.verify(signature)
  }
}

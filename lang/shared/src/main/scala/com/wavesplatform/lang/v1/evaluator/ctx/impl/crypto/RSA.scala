package com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto

import java.security.spec.X509EncodedKeySpec
import java.security.{KeyFactory, Signature}

import com.wavesplatform.common.state.ByteStr
import org.bouncycastle.jce.provider.BouncyCastleProvider

object RSA {

  lazy val bouncyCastleProvider = new BouncyCastleProvider

  sealed trait DigestAlgorithm
  case object NONE    extends DigestAlgorithm
  case object MD5     extends DigestAlgorithm
  case object SHA1    extends DigestAlgorithm
  case object SHA224  extends DigestAlgorithm
  case object SHA256  extends DigestAlgorithm
  case object SHA384  extends DigestAlgorithm
  case object SHA512  extends DigestAlgorithm
  case object SHA3224 extends DigestAlgorithm
  case object SHA3256 extends DigestAlgorithm
  case object SHA3384 extends DigestAlgorithm
  case object SHA3512 extends DigestAlgorithm

  def digestAlgorithmPrefix(alg: DigestAlgorithm): String = {
    alg match {
      case NONE    => "NONE"
      case MD5     => "MD5"
      case SHA1    => "SHA1"
      case SHA224  => "SHA224"
      case SHA256  => "SHA256"
      case SHA384  => "SHA384"
      case SHA512  => "SHA512"
      case SHA3224 => "SHA3-224"
      case SHA3256 => "SHA3-256"
      case SHA3384 => "SHA3-384"
      case SHA3512 => "SHA3-512"
    }
  }

  def verify(digestAlgorithm: DigestAlgorithm, message: ByteStr, signature: Array[Byte], publicKey: Array[Byte]): Boolean = {
    val algorithm = {
      val digestPrefix = digestAlgorithmPrefix(digestAlgorithm)
      s"${digestPrefix}withRSA"
    }

    val publicSignature = Signature.getInstance(algorithm, bouncyCastleProvider)

    val jPublicKeySpec = new X509EncodedKeySpec(publicKey)
    val jPublicKey     = KeyFactory.getInstance("RSA").generatePublic(jPublicKeySpec)

    publicSignature.initVerify(jPublicKey)
    publicSignature.update(message.arr)

    publicSignature.verify(signature)
  }
}

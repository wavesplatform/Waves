package scorex.crypto

import Crypto._

/*
sign & verify functions defined in the same way as in Nxt
 */

object Curve25519Impl {

  def createKeyPair(seed: Array[Byte]): (Array[Byte], Array[Byte]) = {
    val privateKey = new Array[Byte](32)
    val publicKey = new Array[Byte](32)
    Curve25519.keygen(publicKey, privateKey, seed)
    privateKey -> publicKey
  }

  def sign(privateKey: Array[Byte], publicKey: Array[Byte], message: Array[Byte]): Array[Byte] = {
    require(privateKey.length == 32)
    require(publicKey.length == 32)

    val m = sha256(message)
    val x = sha256(m ++ privateKey)

    val Y = new Array[Byte](32)
    Curve25519.keygen(Y, null, x)
    val h = sha256(m ++ Y)

    val v = new Array[Byte](32)
    Curve25519.sign(v, h, x, privateKey)
    v ++ h
  }

  def verify(signature: Array[Byte], message: Array[Byte], publicKey: Array[Byte]): Boolean = {
    require(signature.length == 64)
    require(publicKey.length == 32)

    val v  = new Array[Byte](32)
    val h  = new Array[Byte](32)

    System.arraycopy(signature, 0, v, 0, 32)
    System.arraycopy(signature, 32, h, 0, 32)

    val Y  = new Array[Byte](32)
    Curve25519.verify(Y, v, h, publicKey)

    val m = sha256(message)
    val h2 = sha256(m ++ Y)

    h.sameElements(h2)
  }
}

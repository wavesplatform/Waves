package scorex.account

import scorex.crypto.EllipticCurveImpl

sealed trait PrivateKeyAccount extends PublicKeyAccount {
  def seed: Array[Byte]

  def privateKey: Array[Byte]
}

object PrivateKeyAccount {

  case class PrivateKeyAccountImpl(seed: Array[Byte], privateKey: Array[Byte], publicKey: Array[Byte]) extends PrivateKeyAccount

  def apply(seed: Array[Byte]): PrivateKeyAccount = {
    val pair = EllipticCurveImpl.createKeyPair(seed)
    PrivateKeyAccountImpl(seed, pair._1, pair._2)
  }
}

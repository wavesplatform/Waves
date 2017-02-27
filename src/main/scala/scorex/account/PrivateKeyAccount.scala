package scorex.account

import scorex.crypto.EllipticCurveImpl

case class PrivateKeyAccount(seed: Array[Byte], privateKey: Array[Byte], override val publicKey: Array[Byte])
  extends PublicKeyAccount(publicKey) {
}

object PrivateKeyAccount {
  def apply(seed: Array[Byte]): PrivateKeyAccount = {
    val pair = EllipticCurveImpl.createKeyPair(seed)
    PrivateKeyAccount(seed, pair._1, pair._2)
  }
}

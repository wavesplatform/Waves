package scorex.account

import scorex.crypto.EllipticCurveImpl

case class PrivateKeyAccount(seed: Array[Byte], privateKey: Array[Byte], override val publicKey: Array[Byte])
  extends PublicKeyAccount(publicKey) {

  override val address = Account.fromPubkey(publicKey)

  def this(seed: Array[Byte], keyPair: (Array[Byte], Array[Byte])) = this(seed, keyPair._1, keyPair._2)

  def this(seed: Array[Byte]) = this(seed, EllipticCurveImpl.createKeyPair(seed))
}
package com.wavesplatform.crypto

import java.lang.reflect.Constructor

import org.whispersystems.curve25519.OpportunisticCurve25519Provider

object Curve25519 {
  private lazy val provider: OpportunisticCurve25519Provider = {
    val constructor = classOf[OpportunisticCurve25519Provider].getDeclaredConstructors.head
      .asInstanceOf[Constructor[OpportunisticCurve25519Provider]]
    constructor.setAccessible(true)
    constructor.newInstance()
  }

  val KeyLength: Int = 32

  val SignatureLength: Int = 64

  def privateKeyFromSeed(seed: Array[Byte]): Array[Byte]            = provider.generatePrivateKey(Sha256.hash(seed))
  def publicKeyFromPrivateKey(privateKey: Array[Byte]): Array[Byte] = provider.generatePublicKey(privateKey)

  def createKeyPair(seed: Array[Byte]): (Array[Byte], Array[Byte]) = {
    val sk = privateKeyFromSeed(seed)
    val pk = publicKeyFromPrivateKey(sk)
    (sk, pk)
  }

  def sign(privateKey: Array[Byte], message: Array[Byte]): Array[Byte] =
    provider.calculateSignature(provider.getRandom(SignatureLength), privateKey, message)

  def verify(signature: Array[Byte], message: Array[Byte], publicKey: Array[Byte]): Boolean = provider.verifySignature(publicKey, message, signature)

}

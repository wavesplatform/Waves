package com.wavesplatform.crypto

import org.whispersystems.curve25519.OpportunisticCurve25519Provider

object Curve25519 {
  private lazy val provider = new OpportunisticCurve25519Provider()

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

  def verify(signature: Array[Byte], message: Array[Byte], publicKey: Array[Byte]): Boolean =
    signature != null && signature.length == SignatureLength &&
      publicKey != null && publicKey.length == KeyLength &&
      message != null &&
      provider.verifySignature(publicKey, message, signature)
}

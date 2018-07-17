package com.wavesplatform

import com.wavesplatform.account.PrivateKeyAccount
import scorex.crypto.hash.{Blake2b256, Keccak256}
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey, Signature}

package object crypto {
  val SignatureLength: Int = Curve25519.SignatureLength

  val DigestSize: Int = 32

  def fastHash(m: Array[Byte]): Array[Byte] = Blake2b256.hash(m)

  def fastHash(s: String): Array[Byte] = fastHash(s.getBytes())

  def secureHash(m: Array[Byte]): Array[Byte] = Keccak256.hash(Blake2b256.hash(m))

  def secureHash(s: String): Array[Byte] = secureHash(s.getBytes())

  def sign(account: PrivateKeyAccount, message: Array[Byte]): Array[Byte] =
    Curve25519.sign(PrivateKey(account.privateKey), message)

  def sign(privateKeyBytes: Array[Byte], message: Array[Byte]): Array[Byte] =
    Curve25519.sign(PrivateKey(privateKeyBytes), message)

  def verify(signature: Array[Byte], message: Array[Byte], publicKey: Array[Byte]): Boolean =
    Curve25519.verify(Signature(signature), message, PublicKey(publicKey))

  def createKeyPair(seed: Array[Byte]): (Array[Byte], Array[Byte]) = Curve25519.createKeyPair(seed)
}

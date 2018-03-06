package com.wavesplatform.lang.jvm

import scorex.crypto.signatures.{PublicKey, Signature}

trait Crypto extends com.wavesplatform.lang.traits.Crypto {
  protected def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean =
    scorex.crypto.signatures.Curve25519.verify(Signature(sig), message, PublicKey(pub))
  protected def keccack256(message: Array[Byte]): Array[Byte] = scorex.crypto.hash.Keccak256.hash(message)
  protected def blake2b256(message: Array[Byte]): Array[Byte] = scorex.crypto.hash.Blake2b256.hash(message)
  protected def sha256(message: Array[Byte]): Array[Byte]     = scorex.crypto.hash.Sha256.hash(message)
}

package com.wavesplatform.lang.traits

trait Crypto {
  protected def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean
  protected def keccack256(message: Array[Byte]): Array[Byte]
  protected def blake2b256(message: Array[Byte]): Array[Byte]
  protected def sha256(message: Array[Byte]): Array[Byte]
}

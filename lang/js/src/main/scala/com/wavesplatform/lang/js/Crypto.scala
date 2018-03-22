package com.wavesplatform.lang.js

trait Crypto extends com.wavesplatform.lang.traits.Crypto {
  protected def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean = Global.curve25519verify(message, sig, pub)
  protected def keccack256(message: Array[Byte]): Array[Byte]                                       = Global.keccack256(message).toArray
  protected def blake2b256(message: Array[Byte]): Array[Byte]                                       = Global.blake2b256(message).toArray
  protected def sha256(message: Array[Byte]): Array[Byte]                                           = Global.sha256(message).toArray
}

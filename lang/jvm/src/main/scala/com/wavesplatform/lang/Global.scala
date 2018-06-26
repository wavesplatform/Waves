package com.wavesplatform.lang

import com.wavesplatform.lang.v1.BaseGlobal
import scorex.crypto.encode.Base58
import scorex.crypto.hash.{Blake2b256, Keccak256, Sha256}
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

object Global extends BaseGlobal {
  def base58Encode(input: Array[Byte]): String                 = Base58.encode(input)
  def base58Decode(input: String): Either[String, Array[Byte]] = Base58.decode(input).toEither.left.map(_.getMessage)

  def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean = Curve25519.verify(Signature(sig), message, PublicKey(pub))

  def keccak256(message: Array[Byte]): Array[Byte]  = Keccak256.hash(message)
  def blake2b256(message: Array[Byte]): Array[Byte] = Blake2b256.hash(message)
  def sha256(message: Array[Byte]): Array[Byte]     = Sha256.hash(message)
}

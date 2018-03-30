package com.wavesplatform.lang

import com.wavesplatform.lang.traits.Transaction

object Global extends BaseGlobal {
  def base58Encode(input: Array[Byte]): String                 = impl.Global.base58Encode(input)
  def base58Decode(input: String): Either[String, Array[Byte]] = impl.Global.base58Decode(input)

  def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean = impl.Global.curve25519verify(message, sig, pub)
  def keccack256(message: Array[Byte]): Array[Byte]                                       = impl.Global.keccack256(message)
  def blake2b256(message: Array[Byte]): Array[Byte]                                       = impl.Global.blake2b256(message)
  def sha256(message: Array[Byte]): Array[Byte]                                           = impl.Global.sha256(message)

  def height: Int                                           = impl.Global.height
  def networkByte: Byte                                     = impl.Global.networkByte
  def transaction: Transaction                              = impl.Global.transaction
  def transactionById(id: Array[Byte]): Option[Transaction] = impl.Global.transactionById(id)
}

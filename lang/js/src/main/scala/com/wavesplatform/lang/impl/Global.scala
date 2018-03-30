package com.wavesplatform.lang.impl

import com.wavesplatform.lang.traits.Transaction

import scala.scalajs.js.annotation.JSGlobalScope
import scala.scalajs.{js => platform}

@platform.native
@JSGlobalScope
object Global extends platform.Object {
  def base58Encode(input: Array[Byte]): String                 = platform.native
  def base58Decode(input: String): Either[String, Array[Byte]] = platform.native

  def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean = platform.native
  def keccack256(message: Array[Byte]): Array[Byte]                                       = platform.native
  def blake2b256(message: Array[Byte]): Array[Byte]                                       = platform.native
  def sha256(message: Array[Byte]): Array[Byte]                                           = platform.native

  def height: Int                                           = platform.native
  def networkByte: Byte                                     = platform.native
  def transaction: Transaction                              = platform.native
  def transactionById(id: Array[Byte]): Option[Transaction] = platform.native
}

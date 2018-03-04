package com.wavesplatform.lang.js

import com.wavesplatform.lang.traits.Transaction

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope

@js.native
@JSGlobalScope
object Global extends js.Object {
  def base58Encode(input: Array[Byte]): String    = js.native
  def base58Decode(input: String): js.Array[Byte] = js.native

  def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean = js.native
  def keccack256(message: Array[Byte]): js.Array[Byte]                                    = js.native
  def blake2b256(message: Array[Byte]): js.Array[Byte]                                    = js.native
  def sha256(message: Array[Byte]): js.Array[Byte]                                        = js.native

  def height: Int                                           = js.native
  def transaction: Transaction                              = js.native
  def transactionById(id: Array[Byte]): Option[Transaction] = js.native
}

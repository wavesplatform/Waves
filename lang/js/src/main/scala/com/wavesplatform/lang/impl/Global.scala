package com.wavesplatform.lang.impl

import scala.scalajs.js.annotation.JSGlobalScope
import scala.scalajs.{js => platform}
import scala.scalajs.js.UndefOr
import scala.scalajs.js

@platform.native
@JSGlobalScope
object Global extends platform.Object {
  def base58Encode(input: js.Array[Byte]): String          = platform.native
  def base58Decode(input: String): UndefOr[js.Array[Byte]] = platform.native

  def curve25519verify(message: js.Array[Byte], sig: js.Array[Byte], pub: js.Array[Byte]): Boolean = platform.native
  def keccak256(message: js.Array[Byte]): js.Array[Byte]                                           = platform.native
  def blake2b256(message: js.Array[Byte]): js.Array[Byte]                                          = platform.native
  def sha256(message: js.Array[Byte]): js.Array[Byte]                                              = platform.native

}

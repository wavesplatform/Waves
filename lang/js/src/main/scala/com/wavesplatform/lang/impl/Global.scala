package com.wavesplatform.lang.impl

import scala.scalajs.js.annotation.JSGlobalScope
import scala.scalajs.{js => platform}

@platform.native
@JSGlobalScope
object Global extends platform.Object {
  def base58Encode(input: Array[Byte]): Either[String, String] = platform.native
  def base58Decode(input: String, limit: Int): Either[String, Array[Byte]] = platform.native

  def base64Encode(input: Array[Byte]): Either[String, String] = platform.native
  def base64Decode(input: String, limit: Int): Either[String, Array[Byte]] = platform.native

  def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean = platform.native
  def keccak256(message: Array[Byte]): Array[Byte]                                        = platform.native
  def blake2b256(message: Array[Byte]): Array[Byte]                                       = platform.native
  def sha256(message: Array[Byte]): Array[Byte]                                           = platform.native
}

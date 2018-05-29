package com.wavesplatform.lang

import com.wavesplatform.lang.v1.BaseGlobal
import scalajs.js.JSConverters._

object Global extends BaseGlobal {
  def base58Encode(input: Array[Byte]): String = impl.Global.base58Encode(input.toJSArray)
  def base58Decode(input: String): Either[String, Array[Byte]] =
    impl.Global
      .base58Decode(input)
      .toOption
      .map(_.toArray[Byte])
      .fold[Either[String, Array[Byte]]](Left("Cannot decode"))(Right(_))

  def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean =
    impl.Global.curve25519verify(message.toJSArray, sig.toJSArray, pub.toJSArray)
  def keccak256(message: Array[Byte]): Array[Byte]  = impl.Global.keccak256(message.toJSArray).toArray
  def blake2b256(message: Array[Byte]): Array[Byte] = impl.Global.blake2b256(message.toJSArray).toArray
  def sha256(message: Array[Byte]): Array[Byte]     = impl.Global.sha256(message.toJSArray).toArray
}

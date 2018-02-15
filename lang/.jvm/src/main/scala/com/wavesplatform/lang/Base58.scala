package com.wavesplatform.lang

import scala.util.Try

trait Base58 extends com.wavesplatform.lang.traits.Base58{
  protected val Base58Chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  protected def base58Encode(input: Array[Byte]): String = scorex.crypto.encode.Base58.encode(input)
  protected def base58Decode(input: String): Try[Array[Byte]] = scorex.crypto.encode.Base58.decode(input)
}
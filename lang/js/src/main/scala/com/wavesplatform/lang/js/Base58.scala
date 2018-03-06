package com.wavesplatform.lang.js

import scala.util.{Success, Try}

trait Base58 extends com.wavesplatform.lang.traits.Base58 {
  protected val Base58Chars                                   = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  protected def base58Encode(input: Array[Byte]): String      = Global.base58Encode(input)
  protected def base58Decode(input: String): Try[Array[Byte]] = Success(Global.base58Decode(input).toArray)
}

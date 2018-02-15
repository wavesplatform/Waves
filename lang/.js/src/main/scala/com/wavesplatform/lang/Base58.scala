package com.wavesplatform.lang

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope
import scala.util.{Success, Try}

@js.native
@JSGlobalScope
object JSGlobalScope extends js.Object {
  def base58Encode(input: Array[Byte]): String = js.native
  def base58Decode(input: String): js.Array[Byte] = js.native
}

trait Base58 extends com.wavesplatform.lang.traits.Base58{
  protected val Base58Chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  protected def base58Encode(input: Array[Byte]): String = JSGlobalScope.base58Encode(input)
  protected def base58Decode(input: String): Try[Array[Byte]] = Success(JSGlobalScope.base58Decode(input).toArray)
}

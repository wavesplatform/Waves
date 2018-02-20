package com.wavesplatform.lang.js

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope

@js.native
@JSGlobalScope
object Global extends js.Object {
  def base58Encode(input: Array[Byte]): String = js.native
  def base58Decode(input: String): js.Array[Byte] = js.native
}
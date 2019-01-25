package com.wavesplatform.common.utils

import scala.util.Try

object Base64 {
  def encode(input: Array[Byte]): String = new String(java.util.Base64.getEncoder.encode(input))

  def decode(input: String): Try[Array[Byte]] = Try {
    val str = if (input.startsWith("base64:")) input.substring(7) else input
    java.util.Base64.getDecoder.decode(str)
  }
}

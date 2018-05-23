package com.wavesplatform.utils

import scala.util.Try

object Base64 {
  def encode(input: Array[Byte]): String = "base64:" + new String(java.util.Base64.getEncoder.encode(input))

  def decode(input: String): Try[Array[Byte]] = Try {
    if (input.length < 7) throw new IllegalArgumentException("String of the form base64:chars expected")
    else java.util.Base64.getDecoder.decode(input.substring(7))
  }
}

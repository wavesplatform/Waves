package com.wavesplatform.common.utils

object Base64 extends BaseXXEncDec {
  val Prefix                           = "base64:"
  override val defaultDecodeLimit: Int = 1024 * 1024 * 1024 // 1 MB

  override def encode(input: Array[Byte]): String = {
    val encoder      = java.util.Base64.getEncoder
    val encodedBytes = encoder.encode(input)
    new String(encodedBytes)
  }

  override def decode(input: String): Array[Byte] = {
    val decoder    = java.util.Base64.getDecoder
    val encodedStr = if (input.startsWith(Prefix)) input.substring(Prefix.length) else input
    decoder.decode(encodedStr)
  }
}

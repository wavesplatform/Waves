package com.wavesplatform.common.utils

object Base64 extends BaseXXEncDec {
  val Prefix = "base64:"
  override val defaultDecodeLimit: Int = 1024 * 1024 * 1024 // 1 MB

  override def encode(input: Array[Byte]): String = new String(java.util.Base64.getEncoder.encode(input))

  override def decode(input: String): Array[Byte] = {
    val str = if (input.startsWith(Prefix)) input.substring(Prefix.length) else input
    java.util.Base64.getDecoder.decode(str)
  }
}

package com.wavesplatform.utils

import java.nio.charset.StandardCharsets

object StrUtils {
  def toStringExact(bs: Array[Byte]): String = new String(bs.map(_.toChar))
  def toBytesExact(s: String): Array[Byte]   = s.toCharArray.map(_.toByte)

  def toStringUTF8(bs: Array[Byte]): String = new String(bs, StandardCharsets.UTF_8)
  def toBytesUTF8(s: String): Array[Byte]   = s.getBytes(StandardCharsets.UTF_8)
}

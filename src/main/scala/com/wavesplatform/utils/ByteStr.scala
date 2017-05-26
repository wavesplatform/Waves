package com.wavesplatform.utils

import java.util

import scorex.crypto.encode.Base58

class ByteStr(val bytes: Array[Byte]) {
  override lazy val hashCode = util.Arrays.hashCode(bytes)
  override def equals(obj: Any) = obj match {
    case b: ByteStr => util.Arrays.equals(bytes, b.bytes)
    case _ => false
  }

  override lazy val toString = Base58.encode(bytes)
}

object ByteStr {
  def apply(str: String): ByteStr = new ByteStr(Base58.decode(str).get)
  def apply(bytes: Array[Byte]): ByteStr = new ByteStr(bytes)
}

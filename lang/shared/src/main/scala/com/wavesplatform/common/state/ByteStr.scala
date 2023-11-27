package com.wavesplatform.common.state

import com.wavesplatform.common.utils.{Base58, Base64}

import scala.util.Try

case class ByteStr(arr: Array[Byte]) {
  private[this] lazy val base58: String = Base58.encode(arr)
  lazy val base64Raw: String            = Base64.encode(arr)
  lazy val base64: String               = "base64:" + base64Raw
  lazy val trim: String = (if (arr.length < 1024) {
                             base58.take(7)
                           } else {
                             base64Raw
                           }) + "..."
  override lazy val toString: String = if (arr.length < 1024) {
    base58
  } else {
    base64
  }

  def isEmpty: Boolean = arr.length == 0

  def size: Int = arr.length

  // java replaces invalid chars
  def toUTF8String: String = new String(arr, "UTF-8")

  def ++(other: ByteStr): ByteStr =
    if (this.isEmpty) other else ByteStr(this.arr ++ other.arr)

  def take(n: Int): ByteStr = {
    val n1 = n min arr.length max 0

    if (n1 == arr.length) this
    else if (n1 == 0) ByteStr.empty
    else ByteStr(arr.take(n1))
  }

  def drop(n: Int): ByteStr = {
    val n1 = n min arr.length max 0

    if (n1 == arr.length) ByteStr.empty
    else if (n1 == 0) this
    else ByteStr(arr.drop(n1))
  }

  def takeRight(n: Int): ByteStr = drop(arr.length - n)

  def dropRight(n: Int): ByteStr = take(arr.length - n.max(0))

  override def equals(a: Any): Boolean = a match {
    case other: ByteStr => java.util.Arrays.equals(arr, other.arr)
    case _              => false
  }

  private lazy val hc = java.util.Arrays.hashCode(arr)

  override final def hashCode(): Int = hc
}

object ByteStr {
  val empty: ByteStr = ByteStr(Array.emptyByteArray)

  def fromBytes(bytes: Byte*): ByteStr = ByteStr(bytes.toArray)

  def fromLong(longValue: Long): ByteStr = {
    val buf = new Array[Byte](8)
    var b   = longValue

    for (i <- (buf.length - 1) to 0 by -1) {
      buf(i) = b.toByte
      b = b >> 8
    }

    ByteStr(buf)
  }

  def fill(size: Int)(b: Int): ByteStr = ByteStr(Array.fill(size)(b.toByte))

  def decodeBase58(s: String): Try[ByteStr] = Base58.tryDecodeWithLimit(s).map { bs =>
    ByteStr(bs)
  }

  def decodeBase64(s: String): Try[ByteStr] = Base64.tryDecode(s).map { bs =>
    ByteStr(bs)
  }
}

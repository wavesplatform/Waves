package com.wavesplatform.common.state

import com.wavesplatform.common.utils.{Base58, Base64}

import scala.util.Try

case class ByteStr(arr: Array[Byte]) {
  lazy val base58: String = Base58.encode(arr)

  lazy val base64: String = "base64:" + Base64.encode(arr)

  lazy val trim: String = base58.toString.take(7) + "..."

  override lazy val toString: String = base58

  def isEmpty: Boolean = arr.length == 0

  def ++(other: ByteStr): ByteStr = if (this.isEmpty) other else ByteStr(this.arr ++ other.arr)

  def take(n: Long): ByteStr = {

    val n1 = n min arr.length max 0

    if (n1 == arr.length) this
    else if (n1 == 0) ByteStr.empty
    else {
      ByteStr(arr.take(n1.toInt))
    }

  }

  def drop(n: Long): ByteStr = {

    val n1 = n min arr.length max 0

    if (n1 == arr.length) ByteStr.empty
    else if (n1 == 0) this
    else {
      ByteStr(arr.drop(n1.toInt))
    }
  }

  def takeRight(n: Long): ByteStr = drop(arr.length.toLong - n)

  def dropRight(n: Long): ByteStr = take(arr.length.toLong - n.max(0))

  override def equals(a: Any): Boolean = a match {
    case other: ByteStr => arr.sameElements(other.arr)
    case _              => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(arr)
}

object ByteStr {
  val empty: ByteStr = ByteStr(Array.emptyByteArray)

  implicit def fromByteArray(arr: Array[Byte]): ByteStr = {
    new ByteStr(arr)
  }

  implicit def toByteArray(bs: ByteStr): Array[Byte] = {
    bs.arr
  }

  def fromBytes(bytes: Byte*): ByteStr = {
    ByteStr(bytes.toArray)
  }

  def fromLong(l: Long): ByteStr = {

    val buf = new Array[Byte](8)
    var b   = l

    for (i <- (buf.length - 1) to 0 by -1) {
      buf(i) = b.toByte
      b = b >> 8
    }

    ByteStr(buf)
  }

  def fill(size: Int)(b: Int): ByteStr = ByteStr(Array.fill(size)(b.toByte))

  def decodeBase58(s: String): Try[ByteStr] = Base58.tryDecodeWithLimit(s).map(ByteStr(_))

  def decodeBase64(s: String): Try[ByteStr] = Base64.tryDecode(s).map(ByteStr(_))

  implicit val byteStrOrdering: Ordering[ByteStr] = (x, y) => compare(x.arr, y.arr)

  // scorex.utils.ByteArray.compare
  private def compare(buffer1: Array[Byte], buffer2: Array[Byte]): Int =
    if (buffer1 sameElements buffer2) {
      0
    } else {
      val end1: Int = if (buffer1.length < buffer2.length) buffer1.length else buffer2.length
      var i: Int    = 0
      while (i < end1) {
        val a: Int = buffer1(i) & 0xff
        val b: Int = buffer2(i) & 0xff
        if (a != b) {
          return a - b
        }
        i = i + 1
      }
      buffer1.length - buffer2.length
    }

}

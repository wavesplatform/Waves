package com.wavesplatform.state2

import scorex.crypto.encode.Base58

import scala.util.Try

case class ByteStr(arr: Array[Byte]) {
  override def equals(a: Any): Boolean = a match {
    case other: ByteStr => arr.sameElements(other.arr)
    case _ => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(arr)

  lazy val base58: String = Base58.encode(arr)

  override lazy val toString: String = base58
}

object ByteStr {
  def decodeBase58(s: String): Try[ByteStr] = Base58.decode(s).map(ByteStr(_))
  val empty : ByteStr = ByteStr(Array.emptyByteArray)
}



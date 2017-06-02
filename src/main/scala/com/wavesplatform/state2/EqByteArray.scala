package com.wavesplatform.state2

import scorex.crypto.encode.Base58

import scala.util.Try

case class EqByteArray(arr: Array[Byte]) {
  override def equals(a: Any): Boolean = a match {
    case eba: EqByteArray => arr.sameElements(eba.arr)
    case _ => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(arr)

  lazy val base58: String = Base58.encode(arr)

  override lazy val toString: String = "ByteArray:" + base58
}

object EqByteArray {
  def decode(s: String): Try[EqByteArray] = Base58.decode(s).map(EqByteArray(_))
  val empty : EqByteArray = EqByteArray(Array.emptyByteArray)
}



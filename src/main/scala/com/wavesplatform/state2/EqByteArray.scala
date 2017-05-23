package com.wavesplatform.state2

import scorex.crypto.encode.Base58

case class EqByteArray(arr: Array[Byte]) {
  override def equals(a: Any): Boolean = a match {
    case eba: EqByteArray => arr.sameElements(eba.arr)
    case _ => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(arr)

  override lazy val toString: String = "ByteArray:" + Base58.encode(arr)

}
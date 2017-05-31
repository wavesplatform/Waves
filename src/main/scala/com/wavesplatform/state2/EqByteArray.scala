package com.wavesplatform.state2

import java.nio.ByteBuffer
import shapeless.syntax.typeable._
import org.h2.mvstore.WriteBuffer
import org.h2.mvstore.`type`.DataType
import scorex.crypto.encode.Base58

import org.h2.mvstore.DataUtils

case class EqByteArray(arr: Array[Byte]) {
  override def equals(a: Any): Boolean = a match {
    case eba: EqByteArray => arr.sameElements(eba.arr)
    case _ => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(arr)

  override lazy val toString: String = "ByteArray:" + Base58.encode(arr)

}




package com.wavesplatform.lang.v1.traits.domain

import scodec.bits.ByteVector

trait DataItem[T] {
  val key: String
  val value: T
}

object DataItem {
  case class Lng(k: String, v: Long)       extends DataItem[Long] { val key = k; val value = v }
  case class Bool(k: String, v: Boolean)   extends DataItem[Boolean] { val key = k; val value = v }
  case class Bin(k: String, v: ByteVector) extends DataItem[ByteVector] { val key = k; val value = v }
  case class Str(k: String, v: String)     extends DataItem[String] { val key = k; val value = v }
}

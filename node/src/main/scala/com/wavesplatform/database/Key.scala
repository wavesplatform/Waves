package com.wavesplatform.database

import com.google.common.base.CaseFormat
import com.google.common.io.BaseEncoding
import com.google.common.primitives.{Bytes, Shorts}

abstract class Key[V](prefix: Short, val name: String, val suffix: Array[Byte]) {
  val keyBytes: Array[Byte] = Bytes.concat(Shorts.toByteArray(prefix), suffix)
  def parse(bytes: Array[Byte]): V
  def encode(v: V): Array[Byte]

  override lazy val toString: String = s"$name($prefix,${BaseEncoding.base16().encode(suffix)})"

  override def equals(obj: Any): Boolean = obj match {
    case that: Key[_] => java.util.Arrays.equals(this.keyBytes, that.keyBytes)
    case _            => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(keyBytes)
}

object Key {
  private[this] val converter   = CaseFormat.UPPER_CAMEL.converterTo(CaseFormat.LOWER_HYPHEN)
  private[this] val keyTagToStr = KeyTags.values.map(v => v -> converter.convert(v.toString)).toMap

  def apply[V](keyTag: KeyTags.KeyTag, keySuffix: Array[Byte], parser: Array[Byte] => V, encoder: V => Array[Byte]): Key[V] =
    new Key[V](keyTag.id.toShort, keyTagToStr(keyTag), keySuffix) {
      override def parse(bytes: Array[Byte]): V = parser(bytes)
      override def encode(v: V): Array[Byte]    = encoder(v)
    }

  def opt[V](keyTag: KeyTags.KeyTag, keySuffix: Array[Byte], parser: Array[Byte] => V, encoder: V => Array[Byte]): Key[Option[V]] =
    apply[Option[V]](
      keyTag,
      keySuffix,
      Option(_).map(parser),
      _.fold[Array[Byte]](Array.emptyByteArray)(encoder)
    )
}

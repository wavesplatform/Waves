package com.wavesplatform.database

import com.google.common.base.CaseFormat
import com.google.common.primitives.Bytes

trait Key[V] {
  def name: String
  def keyBytes: Array[Byte]
  def parse(bytes: Array[Byte]): V
  def encode(v: V): Array[Byte]

  override lazy val toString: String = BigInt(keyBytes).toString(16)

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Key[V] => java.util.Arrays.equals(this.keyBytes, that.keyBytes)
    case _            => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(keyBytes)
}

object Key {
  private[this] val converter = CaseFormat.UPPER_CAMEL.converterTo(CaseFormat.LOWER_HYPHEN)

  def apply[V](keyTag: KeyTags.KeyTag, keySuffix: Array[Byte], parser: Array[Byte] => V, encoder: V => Array[Byte]): Key[V] = new Key[V] {
    override val name: String          = converter.convert(keyTag.toString)
    override val keyBytes: Array[Byte] = Bytes.concat(keyTag.prefixBytes, keySuffix)

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

package com.wavesplatform.database

import com.google.common.base.CaseFormat
import com.google.common.io.BaseEncoding
import com.google.common.primitives.{Bytes, Ints, Shorts}
import org.rocksdb.ColumnFamilyHandle

/** @param heightFirst
  *   the suffix starts with a 4-byte height, which goes before the tag, so that keys of several tags in one column family are ordered by height
  */
abstract class Key[V](
    prefix: Short,
    val name: String,
    val suffix: Array[Byte],
    val columnFamilyHandle: Option[ColumnFamilyHandle] = None,
    heightFirst: Boolean = false
) {
  val keyBytes: Array[Byte] =
    if (heightFirst) Bytes.concat(suffix.take(Ints.BYTES), Shorts.toByteArray(prefix), suffix.drop(Ints.BYTES))
    else Bytes.concat(Shorts.toByteArray(prefix), suffix)
  def parse(bytes: Array[Byte]): V
  def encode(v: V): Array[Byte]

  override lazy val toString: String = s"$name($prefix,${BaseEncoding.base16().encode(suffix)})"

  override def equals(obj: Any): Boolean = obj match {
    case that: Key[?] => java.util.Arrays.equals(this.keyBytes, that.keyBytes)
    case _            => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(keyBytes)
}

object Key {
  private val converter   = CaseFormat.UPPER_CAMEL.converterTo(CaseFormat.LOWER_HYPHEN)
  private val keyTagToStr = KeyTag.values.sortBy(_.ordinal).map(v => converter.convert(v.toString))

  def apply[V](
      keyTag: KeyTag,
      keySuffix: Array[Byte],
      parser: Array[Byte] => V,
      encoder: V => Array[Byte],
      cfh: Option[ColumnFamilyHandle] = None,
      heightFirst: Boolean = false
  ): Key[V] =
    new Key[V](keyTag.ordinal.toShort, keyTagToStr(keyTag.ordinal), keySuffix, cfh, heightFirst) {
      override def parse(bytes: Array[Byte]): V = parser(bytes)
      override def encode(v: V): Array[Byte]    = encoder(v)
    }

  def opt[V](
      keyTag: KeyTag,
      keySuffix: Array[Byte],
      parser: Array[Byte] => V,
      encoder: V => Array[Byte],
      cfh: Option[ColumnFamilyHandle] = None,
      heightFirst: Boolean = false
  ): Key[Option[V]] =
    apply[Option[V]](
      keyTag,
      keySuffix,
      Option(_).map(parser),
      _.fold[Array[Byte]](Array.emptyByteArray)(encoder),
      cfh,
      heightFirst
    )
}

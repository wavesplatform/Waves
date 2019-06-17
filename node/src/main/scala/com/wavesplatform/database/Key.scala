package com.wavesplatform.database

import com.google.common.primitives.{Bytes, Ints, Shorts}

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

  override def hashCode(): Int =
    java.util.Arrays.hashCode(keyBytes)
}


object Key {

  final case class KeyPrefix[V](name: String, shortPrefix: Short, bytesPrefix: Array[Byte], parseF: Array[Byte] => V, encodeF: V => Array[Byte]) {
    lazy val stableBytes: Array[Byte] = Bytes.concat(Shorts.toByteArray(shortPrefix), bytesPrefix)

    def withSuffix(suffix: Array[Byte]) = new Key[V] {
      override def name: String = KeyPrefix.this.name

      override def keyBytes: Array[Byte] = Bytes.concat(stableBytes, suffix)

      override def parse(bytes: Array[Byte]): V = parseF(bytes)

      override def encode(v: V): Array[Byte] = encodeF(v)
    }

    def withHeightSuffix(height: Int) = withSuffix(Ints.toByteArray(height))
  }

  def apply[V](keyName: String, key: Array[Byte], parser: Array[Byte] => V, encoder: V => Array[Byte]): Key[V] = new Key[V] {
    override def name: String = keyName

    override def keyBytes: Array[Byte] = key

    override def parse(bytes: Array[Byte]) = parser(bytes)

    override def encode(v: V) = encoder(v)
  }

  def prefix[V](keyName: String, shortPrefix: Short, prefixBytes: Array[Byte], parser: Array[Byte] => V, encoder: V => Array[Byte]) =
    KeyPrefix[V](keyName, shortPrefix, prefixBytes, parser, encoder)

  def opt[V](name: String, key: Array[Byte], parser: Array[Byte] => V, encoder: V => Array[Byte]): Key[Option[V]] =
    apply[Option[V]](name, key, bs => Option(bs).map(parser), _.fold[Array[Byte]](Array.emptyByteArray)(encoder))

  implicit class KeyExt[V](private val key: Key[V]) extends AnyVal {
    def optional = new Key[Option[V]] {
      override def name: String = key.name

      override def keyBytes: Array[Byte] = key.keyBytes

      override def parse(bytes: Array[Byte]): Option[V] = Option(bytes).filter(_.nonEmpty).map(key.parse)

      override def encode(v: Option[V]): Array[Byte] = v.fold(Array.emptyByteArray)(key.encode)
    }
  }
}

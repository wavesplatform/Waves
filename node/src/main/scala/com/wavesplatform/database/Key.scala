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

  final case class PrefixedKey[V](name: String, shortPrefix: Short, bytesPrefix: Array[Byte], suffix: Array[Byte], parseF: Array[Byte] => V, encodeF: V => Array[Byte]) extends Key[V] {
    lazy val stableBytes: Array[Byte] = Bytes.concat(Shorts.toByteArray(shortPrefix), bytesPrefix)
    override lazy val keyBytes: Array[Byte] = Bytes.concat(stableBytes, suffix)

    def withSuffix(suffix: Array[Byte]) = copy(suffix = suffix)

    def withHeightSuffix(height: Int) = withSuffix(Ints.toByteArray(height))

    override def parse(bytes: Array[Byte]): V = parseF(bytes)

    override def encode(v: V): Array[Byte] = encodeF(v)
  }

  def apply[V](keyName: String, key: Array[Byte], parser: Array[Byte] => V, encoder: V => Array[Byte]): Key[V] = new Key[V] {
    override def name: String = keyName

    override def keyBytes: Array[Byte] = key

    override def parse(bytes: Array[Byte]) = parser(bytes)

    override def encode(v: V) = encoder(v)
  }

  def prefixed[V](keyName: String, shortPrefix: Short, prefixBytes: Array[Byte], parser: Array[Byte] => V, encoder: V => Array[Byte]) =
    PrefixedKey[V](keyName, shortPrefix, prefixBytes, Array.emptyByteArray, parser, encoder)

  def opt[V](name: String, key: Array[Byte], parser: Array[Byte] => V, encoder: V => Array[Byte]): Key[Option[V]] =
    apply[Option[V]](name, key, bs => Option(bs).map(parser), _.fold[Array[Byte]](Array.emptyByteArray)(encoder))
}

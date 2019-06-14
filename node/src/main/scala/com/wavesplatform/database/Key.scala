package com.wavesplatform.database

import com.wavesplatform.database.KeyDsl.KeyW

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
  def raw[V](keyName: String, key: Array[Byte], parser: Array[Byte] => V, encoder: V => Array[Byte]): Key[V] = new Key[V] {
    override def name: String = keyName

    override def keyBytes: Array[Byte] = key

    override def parse(bytes: Array[Byte]) = parser(bytes)

    override def encode(v: V) = encoder(v)
  }

  def apply[K: KeyW, V](keyName: String, key: K, parser: Array[Byte] => V, encoder: V => Array[Byte]): Key[V] = {
    raw(keyName, KeyW[K].toBytes(key), parser, encoder)
  }


  def opt[K: KeyW, V](name: String, key: K, parser: Array[Byte] => V, encoder: V => Array[Byte]): Key[Option[V]] =
    apply[K, Option[V]](name, key, bs => Option(bs).map(parser), _.fold[Array[Byte]](Array.emptyByteArray)(encoder))
}

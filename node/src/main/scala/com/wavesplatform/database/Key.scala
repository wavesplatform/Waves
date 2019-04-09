package com.wavesplatform.database

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
  def apply[V](keyName: String, key: Array[Byte], parser: Array[Byte] => V, encoder: V => Array[Byte]): Key[V] = new Key[V] {
    override def name: String = keyName

    override def keyBytes: Array[Byte] = key

    override def parse(bytes: Array[Byte]) = parser(bytes)

    override def encode(v: V) = encoder(v)
  }

  def opt[V](name: String, key: Array[Byte], parser: Array[Byte] => V, encoder: V => Array[Byte]): Key[Option[V]] =
    apply[Option[V]](name, key, Option(_).map(parser), _.fold[Array[Byte]](null)(encoder))
}

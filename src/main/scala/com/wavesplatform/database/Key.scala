package com.wavesplatform.database

trait Key[V] {
  def keyBytes: Array[Byte]

  def parse(bytes: Array[Byte]): V

  def encode(v: V): Array[Byte]

  override lazy val toString: String = BigInt(keyBytes).toString(16)
}

object Key {
  def apply[V](key: Array[Byte], parser: Array[Byte] => V, encoder: V => Array[Byte]): Key[V] = new Key[V] {
    override def keyBytes: Array[Byte] = key

    override def parse(bytes: Array[Byte]) = parser(bytes)

    override def encode(v: V) = encoder(v)
  }

  def opt[V](key: Array[Byte], parser: Array[Byte] => V, encoder: V => Array[Byte]): Key[Option[V]] =
    apply[Option[V]](key, Option(_).map(parser), _.fold[Array[Byte]](null)(encoder))
}

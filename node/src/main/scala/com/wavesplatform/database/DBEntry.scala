package com.wavesplatform.database

import java.util

import com.google.common.primitives.{Ints, Longs, Shorts}
import com.wavesplatform.lang.script.Script

trait DBPair[K, V] {
  def prefix: Array[Byte]
  def encodeKey(key: K): Array[Byte]
  def encodeValue(value: V): Array[Byte]
  def decodeValue(bytes: Array[Byte]): V
  def decode(entry: util.Map.Entry[Array[Byte], Array[Byte]]): (K, V)
}

object Values {
  trait History {
    def decodeValue(bytes: Array[Byte]): Seq[Int] = readIntSeq(bytes)
    def encodeValue(value: Seq[Int]): Array[Byte] = writeIntSeq(value)
  }

  trait LongValue {
    def decodeValue(bytes: Array[Byte]): Long = Longs.fromByteArray(bytes)
    def encodeValue(value: Long): Array[Byte] = Longs.toByteArray(value)
  }

  trait IntValue {
    def decodeValue(bytes: Array[Byte]): Int = Ints.fromByteArray(bytes)
    def encodeValue(value: Int): Array[Byte] = Ints.toByteArray(value)
  }
}

object KKs {
  private var lastCode = 0
  private def nextPrefix: Array[Byte] = {
    require(lastCode < Short.MaxValue)
    lastCode += 1
    Shorts.toByteArray(lastCode.toShort)
  }

  trait AutoPrefix {
    val prefix: Array[Byte] = nextPrefix
  }

  trait BigIntKey[V] extends DBPair[BigInt, V] {
    override def encodeKey(key: BigInt): Array[Byte] = prefix ++ key.toByteArray
    override def decode(entry: util.Map.Entry[Array[Byte], Array[Byte]]): (BigInt, V) =
      BigInt(entry.getKey.drop(prefix.length)) -> decodeValue(entry.getValue)
  }

  trait HeightKey[V] extends DBPair[Int, V] {
    override def encodeKey(key: Int): Array[Byte] = prefix ++ Ints.toByteArray(key)
    override def decode(entry: util.Map.Entry[Array[Byte], Array[Byte]]): (Int, V) =
      Ints.fromByteArray(entry.getKey.drop(prefix.length)) -> decodeValue(entry.getValue)
  }

  trait ByteStrKey[V] extends DBPair[Array[Byte], V] {
    override def encodeKey(key: Array[Byte]): Array[Byte] = prefix ++ key
    override def decode(entry: util.Map.Entry[Array[Byte], Array[Byte]]): (Array[Byte], V) =
      entry.getKey.drop(prefix.length) -> decodeValue(entry.getValue)
  }

  trait AddressHistory extends BigIntKey[Seq[Int]] with Values.History with AutoPrefix
  trait AssetHistory extends ByteStrKey[Seq[Int]] with Values.History with AutoPrefix

  object WavesBalanceHistory extends AddressHistory
  object WavesBalance extends BigIntKey[Long] with Values.LongValue with AutoPrefix

  object AssetBalanceHistory extends ByteStrKey[Seq[Int]] with Values.History with AutoPrefix
  object AssetBalance extends ByteStrKey[Long] with Values.LongValue with AutoPrefix

  object AddressScriptHistory extends AddressHistory
  object AddressScript extends BigIntKey[Option[(Script, Long)]] with AutoPrefix {
    override def encodeValue(value: Option[(Script, Long)]): Array[Byte] = ???
    override def decodeValue(bytes: Array[Byte]): Option[(Script, Long)] = ???
  }

  object AssetInfoHistory extends AssetHistory

  object SponsorshipHistory extends AssetHistory

  object AssetScripHistory extends AssetHistory
}

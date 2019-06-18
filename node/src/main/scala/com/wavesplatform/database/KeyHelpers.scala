package com.wavesplatform.database

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{AddressId, Height, TxNum}

//noinspection ScalaStyle,TypeAnnotation
object KeyHelpers {
  def h(prefix: Short, height: Height) =
    ByteBuffer.allocate(6).putShort(prefix).putInt(height).array()

  def number(prefix: Short, num: Int) =
    ByteBuffer.allocate(6).putShort(prefix).putInt(num).array()

  def hBytes(prefix: Short, bytes: Array[Byte], height: Height) =
    ByteBuffer.allocate(6 + bytes.length).putShort(prefix).put(bytes).putInt(height).array()

  def numBytes(prefix: Short, bytes: Array[Byte], num: Int) =
    ByteBuffer.allocate(6 + bytes.length).putShort(prefix).put(bytes).putInt(num).array()

  def bytes(prefix: Short, bytes: Array[Byte]) =
    ByteBuffer.allocate(2 + bytes.length).putShort(prefix).put(bytes).array()

  def addr(prefix: Short, addressId: Long) = bytes(prefix, AddressId.toBytes(addressId))

  def hash(prefix: Short, hashBytes: ByteStr) = bytes(prefix, hashBytes.arr)

  def hAddr(prefix: Short, addressId: Long, height: Height) = hBytes(prefix, AddressId.toBytes(addressId), height)

  def hNum(prefix: Short, height: Height, num: TxNum) = Bytes.concat(Shorts.toByteArray(prefix), Ints.toByteArray(height), Shorts.toByteArray(num))

  def historyKey(name: String, prefix: Short, b: Array[Byte]) =
    Key(name, bytes(prefix, b), readIntSeq, writeIntSeq).map(_.asInstanceOf[Seq[Height]], (_: Seq[Height]).asInstanceOf[Seq[Int]])

  def intKey(name: String, prefix: Short, default: Int = 0): Key[Int] =
    Key(name, Shorts.toByteArray(prefix), Option(_).fold(default)(Ints.fromByteArray), Ints.toByteArray)

  def longKey(name: String, prefix: Short, default: Long = 0): Key[Long] =
    Key(name, Longs.toByteArray(prefix), Option(_).fold(default)(Longs.fromByteArray), Longs.toByteArray)

  def bytesSeqNr(name: String, prefix: Short, b: Array[Byte], default: Int = 0): Key[Int] =
    Key(name, bytes(prefix, b), Option(_).fold(default)(Ints.fromByteArray), Ints.toByteArray)

  def unsupported[A](message: String): A => Array[Byte] = _ => throw new UnsupportedOperationException(message)
}

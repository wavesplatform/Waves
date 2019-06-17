package com.wavesplatform.database

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import com.wavesplatform.state.{AddressId, Height, TxNum}

//noinspection ScalaStyle,TypeAnnotation
object KeyHelpers {
  def pHeight(prefix: Short, height: Int) =
    ByteBuffer.allocate(6).putShort(prefix).putInt(height).array()

  def pHeightAndNum(prefix: Short, height: Int, num: TxNum) = Bytes.concat(Shorts.toByteArray(prefix), Ints.toByteArray(height), Shorts.toByteArray(num))

  def pBytesAndHeight(prefix: Short, bytes: Array[Byte], height: Int) =
    ByteBuffer.allocate(6 + bytes.length).putShort(prefix).put(bytes).putInt(height).array()

  def pBytes(prefix: Short, bytes: Array[Byte]) =
    ByteBuffer.allocate(2 + bytes.length).putShort(prefix).put(bytes).array()

  def pAddr(prefix: Short, addressId: Long) =
    pBytes(prefix, AddressId.toBytes(addressId))

  def pAddrAndHeight(prefix: Short, addressId: Long, height: Int) =
    pBytesAndHeight(prefix, AddressId.toBytes(addressId), height)

  def pAddrAndAsset(prefix: Short, addressId: AddressId, height: Height, txNum: TxNum) =
    ByteBuffer
      .allocate(12)
      .putShort(prefix)
      .putInt(addressId.toInt)
      .putInt(height)
      .putShort(txNum)
      .array()

  def addrAndHeight(prefix: Short, addressId: Long, height: Int) =
    pBytesAndHeight(prefix, AddressId.toBytes(addressId), height)

  def assetId(height: Height, txNum: TxNum) =
    ByteBuffer.allocate(6).putInt(height).putShort(txNum).array()

  def addrAndAsset(addressId: AddressId, height: Height, txNum: TxNum) =
    ByteBuffer
      .allocate(10)
      .putInt(addressId.toInt)
      .putInt(height)
      .putShort(txNum)
      .array()

  def addrBytes(addressId: Long, bytes: Array[Byte] = Array.emptyByteArray) = Bytes.concat(AddressId.toBytes(addressId), bytes)

  def historyKey(name: String, prefix: Short, b: Array[Byte]) = Key(name, pBytes(prefix, b), readIntSeq, writeIntSeq)

  def intKey(name: String, prefix: Short, default: Int = 0): Key[Int] =
    Key(name, Shorts.toByteArray(prefix), Option(_).fold(default)(Ints.fromByteArray), Ints.toByteArray)

  def longKey(name: String, prefix: Short, default: Long = 0): Key[Long] =
    Key(name, Longs.toByteArray(prefix), Option(_).fold(default)(Longs.fromByteArray), Longs.toByteArray)

  def bytesSeqNr(name: String, prefix: Short, b: Array[Byte], default: Int = 0): Key[Int] =
    Key(name, pBytes(prefix, b), Option(_).fold(default)(Ints.fromByteArray), Ints.toByteArray)

  def unsupported[A](message: String): A => Array[Byte] = _ => throw new UnsupportedOperationException(message)
}

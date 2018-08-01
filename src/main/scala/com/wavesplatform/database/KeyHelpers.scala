package com.wavesplatform.database

import java.nio.ByteBuffer

import com.google.common.primitives.{Ints, Shorts}
import com.wavesplatform.state.ByteStr

object KeyHelpers {
  def h(prefix: Short, height: Int): Array[Byte] =
    ByteBuffer.allocate(6).putShort(prefix).putInt(height).array()

  def hBytes(prefix: Short, height: Int, bytes: Array[Byte]) =
    ByteBuffer.allocate(6 + bytes.length).putShort(prefix).putInt(height).put(bytes).array()

  def bytes(prefix: Short, bytes: Array[Byte]) =
    ByteBuffer.allocate(2 + bytes.length).putShort(prefix).put(bytes).array()

  def addr(prefix: Short, addressId: BigInt) = bytes(prefix, addressId.toByteArray)

  def hash(prefix: Short, hashBytes: ByteStr) = bytes(prefix, hashBytes.arr)

  def hAddr(prefix: Short, height: Int, addressId: BigInt): Array[Byte] = hBytes(prefix, height, addressId.toByteArray)

  def historyKey(prefix: Short, b: Array[Byte]) = Key(bytes(prefix, b), readIntSeq, writeIntSeq)

  def intKey(prefix: Short, default: Int = 0): Key[Int] =
    Key(Shorts.toByteArray(prefix), Option(_).fold(default)(Ints.fromByteArray), Ints.toByteArray)

  def bytesSeqNr(prefix: Short, b: Array[Byte]): Key[Int] =
    Key(bytes(prefix, b), Option(_).fold(0)(Ints.fromByteArray), Ints.toByteArray)

  def unsupported[A](message: String): A => Array[Byte] = _ => throw new UnsupportedOperationException(message)
}

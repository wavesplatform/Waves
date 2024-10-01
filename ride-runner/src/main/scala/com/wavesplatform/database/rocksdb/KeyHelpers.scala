package com.wavesplatform.database.rocksdb

import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import com.wavesplatform.database.AddressId
import com.wavesplatform.state.TxNum
import com.wavesplatform.database.*

import java.nio.ByteBuffer

object KeyHelpers {
  def h(height: Int): Array[Byte] = Ints.toByteArray(height)

  def hBytes(bytes: Array[Byte], height: Int): Array[Byte] =
    ByteBuffer.allocate(4 + bytes.length).put(bytes).putInt(height).array()

  def hAddr(height: Int, addressId: AddressId): Array[Byte] = hBytes(addressId.toByteArray, height)

  def hNum(height: Int, num: TxNum): Array[Byte] = Bytes.concat(Ints.toByteArray(height), Shorts.toByteArray(num))

  def historyKey(keyTag: KeyTags.KeyTag, suffix: Array[Byte]): Key[Seq[Int]] = Key(keyTag, suffix, readIntSeq, writeIntSeq)

  def intKey(keyTag: KeyTags.KeyTag, default: Int = 0): Key[Int] =
    Key(keyTag, Array.emptyByteArray, v => if (v != null && v.length >= Ints.BYTES) Ints.fromByteArray(v) else default, Ints.toByteArray)

  def longKey(keyTag: KeyTags.KeyTag, default: Long = 0): Key[Long] =
    Key(keyTag, Array.emptyByteArray, v => if (v != null && v.length >= Longs.BYTES) Longs.fromByteArray(v) else default, Longs.toByteArray)

  def bytesSeqNr(keyTag: KeyTags.KeyTag, suffix: Array[Byte], default: Int = 0): Key[Int] =
    Key(keyTag, suffix, v => if (v != null && v.length >= Ints.BYTES) Ints.fromByteArray(v) else default, Ints.toByteArray)

  def unsupported[A](message: String): A => Array[Byte] = _ => throw new UnsupportedOperationException(message)
}

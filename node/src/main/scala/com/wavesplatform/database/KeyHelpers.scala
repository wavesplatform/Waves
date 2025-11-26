package com.wavesplatform.database

import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import com.wavesplatform.state
import com.wavesplatform.state.{Height, TxNum}
import org.rocksdb.ColumnFamilyHandle

import java.nio.ByteBuffer

object KeyHelpers {
  def h(height: Height): Array[Byte] = Ints.toByteArray(height.toInt)

  def hBytes(bytes: Array[Byte], height: Height): Array[Byte] =
    intBytes(bytes, height.toInt)

  def intBytes(bytes: Array[Byte], n: Int): Array[Byte] =
    ByteBuffer.allocate(4 + bytes.length).put(bytes).putInt(n).array()

  def hAddr(height: Height, addressId: AddressId): Array[Byte] = hBytes(addressId.toByteArray, height)

  def hNum(height: Height, num: TxNum): Array[Byte] = Bytes.concat(Ints.toByteArray(height.toInt), Shorts.toByteArray(num.toShort))

  def historyKey(keyTag: KeyTag, suffix: Array[Byte]): Key[Seq[Height]] =
    Key(keyTag, suffix, bs => Height.seq(readIntSeq(bs)*), hs => writeIntSeq(Height.ints(hs)))

  def intKey(keyTag: KeyTag, default: Int = 0): Key[Int] =
    Key(keyTag, Array.emptyByteArray, v => if (v != null && v.length >= Ints.BYTES) Ints.fromByteArray(v) else default, Ints.toByteArray)

  def longKey(keyTag: KeyTag, default: Long = 0): Key[Long] =
    Key(keyTag, Array.emptyByteArray, v => if (v != null && v.length >= Longs.BYTES) Longs.fromByteArray(v) else default, Longs.toByteArray)

  def heightKey(keyTag: KeyTag, default: Int = 0): Key[Height] =
    Key(
      keyTag,
      Array.emptyByteArray,
      v => state.Height(if (v != null && v.length >= Ints.BYTES) Ints.fromByteArray(v) else default),
      h => Ints.toByteArray(h.toInt)
    )

  def bytesSeqNr(keyTag: KeyTag, suffix: Array[Byte], default: Int = 0, cfh: Option[ColumnFamilyHandle] = None): Key[Int] =
    Key(keyTag, suffix, v => if (v != null && v.length >= Ints.BYTES) Ints.fromByteArray(v) else default, Ints.toByteArray, cfh)

  def unsupported[A](message: String): A => Array[Byte] = _ => throw new UnsupportedOperationException(message)
}

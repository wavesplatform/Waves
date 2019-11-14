package com.wavesplatform.serialization

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Shorts}
import com.wavesplatform.account.{Address, AddressOrAlias, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.{KeyLength, SignatureLength}
import com.wavesplatform.transaction.{Asset, AssetIdLength}
import scorex.crypto.hash.Digest32

object Deser {

  def serializeBoolean(b: Boolean): Array[Byte] = if (b) Array(1: Byte) else Array(0: Byte)

  def serializeArray(b: Array[Byte]): Array[Byte] = {
    val length = b.length
    if (length.isValidShort)
      Bytes.concat(Shorts.toByteArray(length.toShort), b)
    else
      throw new IllegalArgumentException(s"Attempting to serialize array with size, but the size($length) exceeds MaxShort(${Short.MaxValue})")
  }

  def parseArrayWithLength(bytes: ByteBuffer): Array[Byte] = {
    val length = bytes.getShort
    require(length >= 0, s"Array length should be non-negative, but $length found")
    val array = new Array[Byte](length)
    bytes.get(array)
    array
  }

  def parseArrayWithLength(bytes: Array[Byte], position: Int): (Array[Byte], Int) = {
    val from   = position + 2
    val length = Shorts.fromByteArray(bytes.slice(position, from))
    val to     = from + length
    require(length >= 0, s"Array length should be non-negative, but $length found")
    require(bytes.length >= to, s"Array length = ${bytes.length} less than slice end point index = $to")
    (bytes.slice(from, to), to)
  }

  def parseArrayByLength(bytes: Array[Byte], position: Int, length: Int): (Array[Byte], Int) = {
    (bytes.slice(position, position + length), position + length)
  }

  def parseByteArrayOption(bytes: Array[Byte], position: Int, length: Int): (Option[Array[Byte]], Int) = {
    if (bytes.slice(position, position + 1).head == (1: Byte)) {
      val b = bytes.slice(position + 1, position + 1 + length)
      (Some(b), position + 1 + length)
    } else (None, position + 1)
  }

  def parseByteArrayOptionWithLength(bytes: ByteBuffer): Option[Array[Byte]] = {
    if (bytes.get() == 1) Some(parseArrayWithLength(bytes)) else None
  }

  def parseOption[T](bytes: Array[Byte], position: Int, length: Int = -1)(deser: Array[Byte] => T): (Option[T], Int) = {
    if (bytes.slice(position, position + 1).head == (1: Byte)) {
      val (arr, arrPosEnd) =
        if (length < 0) {
          parseArrayWithLength(bytes, position + 1)
        } else {
          parseArrayByLength(bytes, position + 1, length)
        }
      (Some(deser(arr)), arrPosEnd)
    } else (None, position + 1)
  }

  def parseArrays(bytes: Array[Byte]): Seq[Array[Byte]] = {
    val arraysCount = Shorts.fromByteArray(bytes.slice(0, 2))
    require(arraysCount >= 0, s"Arrays count should be non-negative, but $arraysCount found")
    require(
      arraysCount <= (bytes.length - 2) / 2,
      s"Bytes with length = ${bytes.length - 2} can't contain $arraysCount array(s)"
    )
    val r = (0 until arraysCount).foldLeft((Seq.empty[Array[Byte]], 2)) {
      case ((acc, pos), _) =>
        val (arr, nextPos) = parseArrayWithLength(bytes, pos)
        (acc :+ arr, nextPos)
    }
    r._1
  }

  def serializeOption[T](b: Option[T])(ser: T => Array[Byte]): Array[Byte] =
    b.map(a => (1: Byte) +: ser(a)).getOrElse(Array(0: Byte))

  def serializeOptionOfArrayWithLength[T](b: Option[T])(ser: T => Array[Byte]): Array[Byte] =
    b.map(a => (1: Byte) +: serializeArray(ser(a))).getOrElse(Array(0: Byte))

  def serializeArrays(bs: Seq[Array[Byte]]): Array[Byte] = {
    require(bs.length.isValidShort, s"Attempting to serialize array with size, but the size(${bs.length}) exceeds MaxShort(${Short.MaxValue})")
    val countBytes = Shorts.toByteArray(bs.length.toShort)
    Bytes.concat(Seq(countBytes) ++ bs.map(serializeArray): _*)
  }

  def serializeMerkleRootHash(d: Digest32): Array[Byte] = {
    if (d.isEmpty) Array(0: Byte)
    else (1: Byte) +: d
  }

  def parseMerkleRootHash(bytes: Array[Byte], position: Int): (Digest32, Int) = {
    if (bytes.slice(position, position + 1).head == (1: Byte)) {
      val b = bytes.slice(position + 1, position + 1 + 32)
      (Digest32 @@ b, position + 1 + 32)
    } else (Digest32 @@ Array.emptyByteArray, position + 1)
  }

  implicit class ByteBufferOps(val buf: ByteBuffer) extends AnyVal {
    def getPrefixedByteArray: Array[Byte] = {
      val prefix = buf.getShort
      require(prefix >= 0, "negative array length")
      if (prefix > 0) getByteArray(prefix) else Array.emptyByteArray
    }

    def getAsset: Asset = {
      val prefix = buf.get
      if (prefix == 0) Asset.Waves
      else if (prefix == 1) Asset.IssuedAsset(ByteStr(getByteArray(AssetIdLength)))
      else throw new IllegalArgumentException(s"Invalid asset id prefix: $prefix")
    }

    def getAddressOrAlias: AddressOrAlias = {
      val prefix = buf.get(buf.position())
      prefix match {
        case Address.AddressVersion =>
          Address.fromBytes(getByteArray(Address.AddressLength)).explicitGet()
        case Alias.AddressVersion =>
          val length = buf.getShort(buf.position() + 2)
          Alias.fromBytes(getByteArray(length + 4)).explicitGet()
        case _ => throw new IllegalArgumentException(s"Invalid address or alias prefix: $prefix")
      }
    }

    def getByteArray(size: Int): Array[Byte] = {
      val result = new Array[Byte](size)
      buf.get(result)
      result
    }

    def getShortArray(size: Int): Array[Short] = {
      val result = new Array[Short](size)
      buf.asShortBuffer().get(result)
      buf.position(buf.position() + Shorts.BYTES * size)
      result
    }

    def getSignature: Array[Byte] = getByteArray(SignatureLength)

    def getPublicKey: PublicKey = PublicKey(getByteArray(KeyLength))
  }

}

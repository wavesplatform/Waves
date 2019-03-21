package com.wavesplatform.common.utils
import java.nio.charset.StandardCharsets.US_ASCII

import scala.annotation.tailrec
import scala.util.Try

//noinspection ScalaStyle
object FastBase58 {
  private[this] val Alphabet: Array[Byte] = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".getBytes(US_ASCII)

  private[this] val DecodeTable: Array[Byte] = Array(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, -1, -1, -1, -1, -1, -1, -1,
    9, 10, 11, 12, 13, 14, 15, 16, -1, 17, 18, 19, 20, 21, -1, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, -1, -1, -1, -1, -1, -1, 33, 34, 35, 36, 37,
    38, 39, 40, 41, 42, 43, -1, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57)

  def encode(bin: Array[Byte]): String = {
    if (bin.isEmpty) return ""

    val zeroCount = bin
      .takeWhile(_ == 0)
      .length

    val bufferSize = (bin.length - zeroCount) * 138 / 100 + 1
    val buffer = new Array[Byte](bufferSize)

    var high = bufferSize - 1
    for (index <- zeroCount until bin.length) {
      var endIndex = bufferSize - 1
      var carry = java.lang.Byte.toUnsignedInt(bin(index))

      while (endIndex > high || carry != 0) {
        carry = carry + (256 * java.lang.Byte.toUnsignedInt(buffer(endIndex)))
        buffer(endIndex) = (carry % 58).toByte
        carry /= 58
        endIndex -= 1
      }
      high = endIndex
    }

    val startIndex = buffer
      .takeWhile(_ == 0)
      .length

    val base58Output = new Array[Byte](bufferSize - startIndex + zeroCount)
    for (i <- 0 until zeroCount) base58Output(i) = Alphabet(0)

    val bufferZeroCount = buffer.takeWhile(_ == 0).length
    for (bufferIndex <- bufferZeroCount until bufferSize)
      base58Output(zeroCount + bufferIndex - bufferZeroCount) = Alphabet(java.lang.Byte.toUnsignedInt(buffer(bufferIndex)))

    new String(base58Output, US_ASCII)
  }

  def decode(str: String): Array[Byte] = {
    if (str.isEmpty) return Array.emptyByteArray

    val b58Chars = str.toCharArray
    val outArrayLength = (b58Chars.length + 3) / 4

    var bytesLeft = b58Chars.length % 4
    var zeroMask = 0
    if (bytesLeft > 0) zeroMask = 0xffffffff << (bytesLeft*8)
    else bytesLeft = 4

    val outLongs = new Array[Long](outArrayLength)
    for (r <- b58Chars) {
      if (r >= DecodeTable.length || DecodeTable(r) == -1)  throw new IllegalArgumentException(s"Invalid base58 digit $r")
      var base58EncMask = java.lang.Byte.toUnsignedLong(DecodeTable(r))
      for (outIndex <- (outArrayLength - 1) until 0 by -1) {
        val longValue = outLongs(outIndex)*58 + base58EncMask
        base58EncMask = (longValue >>> 32) & 0x3fL
        outLongs(outIndex) = longValue & 0xffffffffL
      }

      if (base58EncMask > 0) throw new IllegalArgumentException("Output number too big (carry to the next int32)")
      if ((outLongs(0) & zeroMask) != 0) throw new IllegalArgumentException("Output number too big (last int32 filled too far)")
    }

    val outBytes = new Array[Byte]((b58Chars.length + 3) * 3)
    var outBytesCount = 0
    for (j <- 0 until outArrayLength) {
      var mask = (((bytesLeft-1) & 0xff) * 8).toByte
      while (java.lang.Byte.toUnsignedInt(mask) <= 0x18) {
        outBytes(outBytesCount) = (outLongs(j) >>> mask).toByte
        mask = (mask - 8).toByte
        outBytesCount += 1
      }
      if (j == 0) bytesLeft = 4
    }

    val outBytesStart: Int = {
      val zeroCount = b58Chars
        .takeWhile(_ == '1')
        .length

      @tailrec
      def findStart(start: Int = 0): Int = {
        if (start >= outBytes.length) return 0
        val element = outBytes(start)
        if (element != 0) (start - zeroCount) max 0
        else findStart(start + 1)
      }

      findStart()
    }

    java.util.Arrays.copyOfRange(outBytes, outBytesStart, outBytesCount)
  }

  def tryDecode(str: String, limit: Int = 192): Try[Array[Byte]] = Try {
    require(str.length <= limit, s"base58Decode input exceeds $limit")
    decode(str)
  }
}

package com.wavesplatform.common.utils
import java.nio.charset.StandardCharsets.US_ASCII

//noinspection ScalaStyle
object FastBase58 {
  private[this] val Alphabet: Array[Byte] = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".getBytes(US_ASCII)

  private[this] val DecodeTable: Array[Byte] = Array(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, -1, -1, -1, -1, -1, -1, -1,
    9, 10, 11, 12, 13, 14, 15, 16, -1, 17, 18, 19, 20, 21, -1, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, -1, -1, -1, -1, -1, -1, 33, 34, 35, 36, 37,
    38, 39, 40, 41, 42, 43, -1, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57)

  def encode(bin: Array[Byte]): String = {
    val zeroCount = bin.indices
      .dropWhile(i => bin(i) == 0)
      .headOption
      .getOrElse(bin.length)

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

    val startIndex = (0 until bufferSize)
      .dropWhile(j => buffer(j) == 0)
      .headOption
      .getOrElse(bufferSize - 1)

    val base58Output = new Array[Byte](bufferSize - startIndex + zeroCount)

    if (zeroCount != 0) for (i <- 0 until zeroCount) base58Output(i) = '1'.toByte

    for (j <- zeroCount until bufferSize) {
      base58Output(startIndex - zeroCount) = Alphabet(java.lang.Byte.toUnsignedInt(buffer(j)))
    }

    new String(base58Output, US_ASCII)
  }

  def decode(str: String): Array[Byte] = {
    if (str.isEmpty) throw new IllegalArgumentException("Zero length string")

    val b58Chars = str.toCharArray
    val outArrayLength = (b58Chars.length + 3) / 4

    var bytesLeft = b58Chars.length % 4
    var zeroMask = 0
    if (bytesLeft > 0) zeroMask = 0xffffffff << (bytesLeft*8)
    else bytesLeft = 4

    val outLongs = new Array[Long](outArrayLength)
    for (r <- b58Chars) {
      if (r > 127) throw new IllegalArgumentException("High-bit set on invalid digit")
      if (DecodeTable(r) == -1)  throw new IllegalArgumentException("Invalid base58 digit (%q)")
      var c = java.lang.Byte.toUnsignedLong(DecodeTable(r))
      for (j <- (outArrayLength - 1) until 0 by -1) {
        val t = outLongs(j)*58 + c
        c = (t >>> 32) & 0x3f
        outLongs(j) = t & 0xffffffff
      }

      if (c > 0) throw new IllegalArgumentException("Output number too big (carry to the next int32)")
      if ((outLongs(0) & zeroMask) != 0) throw new IllegalArgumentException("Output number too big (last int32 filled too far)")
    }

    val outBytes = new Array[Byte]((b58Chars.length + 3) * 3)
    var outBytesCount = 0
    for (j <- 0 until outArrayLength) {
      var mask = (((bytesLeft-1) & 0xff) * 8).toByte
      while (java.lang.Byte.toUnsignedInt(mask) <= 0x18) {
        outBytes(outBytesCount) = (outLongs(j) >> mask).toByte
        mask = (mask - 8).toByte
        outBytesCount += 1
      }
      if (j == 0) bytesLeft = 4
    }

    val zeroCount = b58Chars.indices
      .dropWhile(i => b58Chars(i) == '1')
      .headOption
      .getOrElse(b58Chars.length - 1)

    for ((v, n) <- outBytes.zipWithIndex) {
      if (v != 0) {
        var start = n - zeroCount.toInt
        if (start < 0) start = 0
        return outBytes.slice(start, outBytesCount)
      }
    }

    outBytes.take(outBytesCount)
  }
}

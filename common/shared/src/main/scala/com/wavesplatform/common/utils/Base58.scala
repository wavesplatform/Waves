package com.wavesplatform.common.utils

import java.util.Arrays

import scala.util.Try

//noinspection ScalaStyle
object Base58 {
  import java.nio.charset.StandardCharsets.US_ASCII

  private[this] val Alphabet: Array[Byte] = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".getBytes(US_ASCII)

  private[this] val DecodeTable: Array[Byte] = Array(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, -1, -1, -1, -1, -1, -1, -1,
    9, 10, 11, 12, 13, 14, 15, 16, -1, 17, 18, 19, 20, 21, -1, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, -1, -1, -1, -1, -1, -1, 33, 34, 35, 36, 37,
    38, 39, 40, 41, 42, 43, -1, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57)

  def encode(bytes: Array[Byte]): String = {
    val input     = Arrays.copyOf(bytes, bytes.length)
    val zeroCount = input.takeWhile(_ == 0).length

    var in                  = zeroCount
    var out                 = input.length * 2
    val output: Array[Byte] = new Array[Byte](out)
    while (in < input.length) {
      val mod = convert(input, in, 256, 58)
      if (input(in) == 0) in += 1
      out -= 1
      output(out) = Alphabet(mod)
    }

    while (out < output.length && output(out) == Alphabet(0)) out += 1
    for (i <- 0 until zeroCount) {
      out -= 1
      output(out) = Alphabet(0)
    }

    new String(output, out, output.length - out, US_ASCII)
  }

  def fastEncode(bin: Array[Byte]): String = {
    val binsz = bin.length
    var i, j, high, zcount, carry = 0

    while (zcount < binsz && bin(zcount) == 0) zcount += 1

    val size = (binsz-zcount)*138/100 + 1
    val buf = new Array[Byte](size)

    high = size - 1
    i = zcount
    while (i < binsz) {
      j = size - 1
      carry = java.lang.Byte.toUnsignedInt(bin(i))

      while (j > high || carry != 0) {
        carry = carry + (256 * java.lang.Byte.toUnsignedInt(buf(j)))
        buf(j) = (carry % 58).toByte
        carry /= 58
        j -= 1
      }
      high = j
      i += 1
    }

    j = 0
    while (j < size && buf(j) == 0) j += 1

    val b58 = new Array[Byte](size - j + zcount)

    if (zcount != 0) {
      i = 0
      while (i < zcount) {
        b58(i) = '1'.toByte
        i += 1
      }
    }

    i = zcount
    while (j < size) {
      b58(i) = Alphabet(java.lang.Byte.toUnsignedInt(buf(j)))
      j += 1
      i += 1
    }

    new String(b58, US_ASCII)
  }

  def decode(string: String, limit: Int = 192 /* 140*log(256)/log(58) */ ): Try[Array[Byte]] = Try {
    val input: Array[Byte] = new Array[Byte](string.length)
    string.length.ensuring(_ <= limit, s"base58Decode input exceeds $limit")
    for (i <- 0 until string.length)
      input(i) = toBase58(string(i)).ensuring(_ != -1, s"Wrong char '${string(i)}' in Base58 string '$string'")

    val zeroCount = input.takeWhile(_ == 0).length

    var in     = zeroCount
    var out    = input.length
    val output = new Array[Byte](out)
    while (in < input.length) {
      val mod = convert(input, in, 58, 256)
      if (input(in) == 0) in += 1
      out -= 1
      output(out) = mod
    }

    while (out < output.length && output(out) == 0) out += 1
    Arrays.copyOfRange(output, out - zeroCount, output.length)
  }

  private[this] def toBase58(c: Char): Byte = if (c < DecodeTable.length) DecodeTable(c) else -1

  private[this] def convert(number: Array[Byte], offset: Int, from: Int, to: Int): Byte = {
    var rem = 0
    var i   = offset
    while (i < number.length) {
      val digit = number(i) & 0xff
      val tmp   = rem * from + digit
      number(i) = (tmp / to).toByte
      rem = tmp % to
      i += 1
    }
    rem.toByte
  }
}

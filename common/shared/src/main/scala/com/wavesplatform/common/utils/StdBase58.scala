package com.wavesplatform.common.utils

import java.util.Arrays

object StdBase58 extends BaseXXEncDec {
  import java.nio.charset.StandardCharsets.US_ASCII

  private val Alphabet: Array[Byte] = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".getBytes(US_ASCII)

  private val DecodeTable: Array[Byte] = Array(
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, -1, -1, -1, -1, -1, -1, -1, 9, 10, 11, 12, 13, 14, 15, 16, -1, 17,
    18, 19, 20, 21, -1, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, -1, -1, -1, -1, -1, -1, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, -1, 44, 45,
    46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57
  )

  private def toBase58(c: Char): Byte = if (c < DecodeTable.length) DecodeTable(c) else -1

  override def defaultDecodeLimit: Int = 192 /* 140*log(256)/log(58) */

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

  def decode(string: String): Array[Byte] = {
    val input: Array[Byte] = new Array[Byte](string.length)

    for (i <- 0 until string.length) {
      input(i) = toBase58(string(i))
      require(input(i) != -1, s"Wrong char '${string(i)}' in Base58 string '$string'")
    }

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

  private def convert(number: Array[Byte], offset: Int, from: Int, to: Int): Byte = {
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

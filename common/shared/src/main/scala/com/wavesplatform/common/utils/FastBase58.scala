package com.wavesplatform.common.utils
import java.nio.charset.StandardCharsets.US_ASCII

object FastBase58 {
  private[this] val Alphabet: Array[Byte] = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".getBytes(US_ASCII)

  private[this] val DecodeTable: Array[Byte] = Array(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, -1, -1, -1, -1, -1, -1, -1,
    9, 10, 11, 12, 13, 14, 15, 16, -1, 17, 18, 19, 20, 21, -1, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, -1, -1, -1, -1, -1, -1, 33, 34, 35, 36, 37,
    38, 39, 40, 41, 42, 43, -1, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57)

  def encode(bin: Array[Byte]): String = {
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

  def decode(str: String): Array[Byte] = {
    if (str.isEmpty) throw new IllegalArgumentException("zero length string")

    var t, zcount = 0L
    var zmask = 0
    val b58u = str.toCharArray
    val b58sz = b58u.length
    val outisz = (b58sz + 3) / 4
    val binu = new Array[Byte]((b58sz+3)*3)
    var bytesleft = b58sz % 4

    if (bytesleft > 0) {
      zmask = 0xffffffff << (bytesleft*8)
    } else {
      bytesleft = 4
    }

    val outi = new Array[Long](outisz)

    var i = 0
    while (i < b58sz && b58u(i) == '1') {
      zcount += 1
      i += 1
    }

    for (r <- b58u) {
      if (r > 127) throw new IllegalArgumentException("High-bit set on invalid digit")
      if (DecodeTable(r) == -1)  throw new IllegalArgumentException("Invalid base58 digit (%q)")
      var c = java.lang.Byte.toUnsignedLong(DecodeTable(r))
      var j = (outisz - 1)
      while (j >= 0) {
        t = outi(j)*58 + c
        c = (t >>> 32) & 0x3f
        outi(j) = t & 0xffffffff
        j -= 1
      }

      if (c > 0) throw new IllegalArgumentException("Output number too big (carry to the next int32)")
      if ((outi(0) & zmask) != 0) throw new IllegalArgumentException("Output number too big (last int32 filled too far)")
    }

    var j, cnt = 0
    while (j < outisz) {
      var mask = (((bytesleft-1) & 0xff) * 8).toByte
      while (java.lang.Byte.toUnsignedInt(mask) <= 0x18) {
        binu(cnt) = (outi(j) >> mask).toByte
        mask = (mask - 8).toByte
        cnt += 1
      }
      if (j == 0) bytesleft = 4
      j += 1
    }

    for ((v, n) <- binu.zipWithIndex) {
      if (v != 0) {
        var start = n - zcount.toInt
        if (start < 0) start = 0
        return binu.slice(start, cnt)
      }
    }

    binu.take(cnt)
  }
}

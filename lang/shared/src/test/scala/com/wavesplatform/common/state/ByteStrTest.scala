package com.wavesplatform.common.state

import com.wavesplatform.common.utils.{Base58, Base64}
import org.scalatest.*

class ByteStrTest extends wordspec.AnyWordSpec with matchers.should.Matchers {

  private def getSeqBytesArr(size: Int, from: Int = 1): Array[Byte] = (from until (from + size) map (_.toByte)).toArray

  "ByteStr" should {

    "correctly serialize int/boolean values" in {
      ByteStr.fromBytes(1).arr shouldBe Array[Byte](1)               // ByteVector(1)
      ByteStr.fromBytes(-100).arr shouldBe Array[Byte](-100)         // ByteVector(-100)
      ByteStr.fromBytes(Byte.MaxValue).arr shouldBe Array[Byte](127) // ByteVector(Byte.MaxValue.toInt)
    }

    "correctly serialize long values" in {
      ByteStr.fromLong(0x0102030405060708L).arr shouldBe Array[Byte](1, 2, 3, 4, 5, 6, 7, 8)        // ByteVector.fromLong(0x0102030405060708L)
      ByteStr.fromLong(33L).arr shouldBe Array[Byte](0, 0, 0, 0, 0, 0, 0, 33)                       // ByteVector.fromLong(33L)
      ByteStr.fromLong(Int.MaxValue.toLong).arr shouldBe Array[Byte](0, 0, 0, 0, 127, -1, -1, -1)   // ByteVector.fromLong(Int.MaxValue.toLong)
      ByteStr.fromLong(Int.MinValue.toLong).arr shouldBe Array[Byte](-1, -1, -1, -1, -128, 0, 0, 0) // ByteVector.fromLong(Int.MaxValue.toLong)
    }

    "be correctly created via fill method" in {
      ByteStr.fill(5)(0).arr shouldBe Array[Byte](0, 0, 0, 0, 0) // ByteVector.fill(5)(0)
    }

    "be correctly concatenated with another one" in {
      ByteStr(Array[Byte](1, 2, 3)) ++ ByteStr(Array[Byte](4, 5, 6)) shouldBe ByteStr(
        getSeqBytesArr(6)
      ) // ByteVector(Array[Byte](1, 2, 3)) ++ ByteVector(Array[Byte](4, 5, 6))
    }

    "correctly take several bytes" in {
      ByteStr(getSeqBytesArr(10)).take(6) shouldBe ByteStr(getSeqBytesArr(6))
      ByteStr(getSeqBytesArr(10)).take(Int.MaxValue) shouldBe ByteStr(getSeqBytesArr(10))
      ByteStr(getSeqBytesArr(10)).take(Int.MinValue) shouldBe ByteStr.empty
    }

    "correctly drop several bytes" in {
      ByteStr(getSeqBytesArr(10)).drop(6) shouldBe ByteStr(Array[Byte](7, 8, 9, 10))
      ByteStr(getSeqBytesArr(10)).drop(Int.MaxValue) shouldBe ByteStr.empty
      ByteStr(getSeqBytesArr(10)).drop(Int.MinValue) shouldBe ByteStr(getSeqBytesArr(10))
    }

    "correctly takeRight several bytes" in {
      ByteStr(getSeqBytesArr(10)).takeRight(6) shouldBe ByteStr(Array[Byte](5, 6, 7, 8, 9, 10))
      ByteStr(getSeqBytesArr(3)).takeRight(-100) shouldBe ByteStr.empty
      ByteStr(getSeqBytesArr(3)).takeRight(100) shouldBe ByteStr(getSeqBytesArr(3))
    }

    "correctly dropRight several bytes" in {
      ByteStr(getSeqBytesArr(10)).dropRight(6) shouldBe ByteStr(Array[Byte](1, 2, 3, 4))
      ByteStr(getSeqBytesArr(3)).dropRight(-100) shouldBe ByteStr(getSeqBytesArr(3))
      ByteStr(getSeqBytesArr(3)).dropRight(100) shouldBe ByteStr.empty
    }

    "serialize to base64 if huge" in {
      val arr      = new Array[Byte](1024)
      val expected = "base64:" + Base64.encode(arr)
      ByteStr(arr).toString shouldBe expected
    }

    "serialize to base58 if small" in {
      val arr      = new Array[Byte](1023)
      val expected = Base58.encode(arr)
      ByteStr(arr).toString shouldBe expected
    }

    "trim using base64 if huge" in {
      val arr      = new Array[Byte](1024)
      val expected = Base64.encode(arr) + "..."
      ByteStr(arr).trim shouldBe expected
    }

    "trim using base64 if small" in {
      val arr      = new Array[Byte](1023)
      val expected = Base58.encode(arr).take(7) + "..."
      ByteStr(arr).trim shouldBe expected
    }
  }
}

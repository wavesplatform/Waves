package com.wavesplatform.serialization

import com.wavesplatform.test.PropSpec

class DeserTest extends PropSpec {
  property("correctly serialized arrays") {
    val arrays: Seq[Array[Byte]] = Seq(Array(1, 2, 3), Array(), Array(1, 2))
    val bytes                    = Deser.serializeArrays(arrays)
    Deser.parseArrays(bytes).toArray should be(arrays.toArray)
  }

  property("too big arrays count") {
    checkDeserError(
      "Bytes with length = 4 can't contain 100 array(s)",
      Array(0, 100, 1, 2, 3, 4)
    )
  }

  property("negative arrays count") {
    checkDeserError(
      "Arrays count should be non-negative, but -2 found",
      Array(-1, -2, 1, 2, 3, 4)
    )
  }

  property("too big array length") {
    checkDeserError(
      "Array length = 10 less than slice end point index = 11",
      Array(0, 2, 0, 2, 1, 2, 0, 3, 1, 2)
    )
  }

  property("negative array length") {
    checkDeserError(
      "Array length should be non-negative, but -3 found",
      Array(0, 2, 0, 2, 1, 2, -1, -3, 1, 2)
    )
  }

  private def checkDeserError(rule: String, data: Array[Byte]): Unit = {
    val thrown = the[IllegalArgumentException] thrownBy Deser.parseArrays(data)
    thrown.getMessage shouldBe s"requirement failed: $rule"
  }
}

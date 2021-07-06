package com.wavesplatform.utils

import com.wavesplatform.common.utils.{Base58, FastBase58, StdBase58}
import com.wavesplatform.test.PropSpec
import org.scalacheck.Gen

class Base58Test extends PropSpec {
  import org.scalacheck.Shrink
  implicit val noShrink: Shrink[String] = Shrink.shrinkAny

  private val Base58Chars  = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  private val IllegalChars = "+/=-_0lIO"

  val base58Gen: Gen[String] =
    for {
      length <- Gen.chooseNum(20, 100)
      chars  <- Gen.listOfN(length, Gen.oneOf(Base58Chars))
    } yield chars.mkString

  val nonBase58Gen: Gen[String] =
    for {
      length <- Gen.chooseNum(20, 100)
      chars <- Gen
        .listOfN(length, Gen.oneOf(Base58Chars ++ IllegalChars))
        .filter(_.toSet.intersect(IllegalChars.toSet).nonEmpty)
    } yield chars.mkString

  property("decodes the same as fast implementation") {
    forAll(base58Gen) { s =>
      val bytes     = StdBase58.decode(s)
      val fastBytes = FastBase58.decode(s)
      bytes.sameElements(fastBytes) shouldBe true

      val str     = StdBase58.encode(bytes)
      val fastStr = FastBase58.encode(bytes)
      str shouldBe fastStr
    }
  }

  property("encodes the same as fast implementation") {
    forAll(base58Gen) { s =>
      val bytes   = StdBase58.decode(s)
      val str     = StdBase58.encode(bytes)
      val fastStr = FastBase58.encode(bytes)
      str shouldBe fastStr
    }
  }

  property("handles zeroes at start") {
    val encodedString = "11WH5tQgZH6Djm7RS2guC"
    val bytes         = Base58.decode(encodedString)

    val stdStr  = StdBase58.encode(bytes)
    val fastStr = FastBase58.encode(bytes)

    stdStr shouldBe fastStr
  }

  property("handles empty sequences") {
    StdBase58.encode(Array.emptyByteArray) shouldBe ""
    FastBase58.encode(Array.emptyByteArray) shouldBe ""

    StdBase58.decode("").toSeq shouldBe Nil
    FastBase58.decode("").toSeq shouldBe Nil
  }

  property("decoding fails on illegal characters") {
    forAll(nonBase58Gen) { invalidStr =>
      intercept[IllegalArgumentException](StdBase58.decode(invalidStr))
      intercept[IllegalArgumentException](FastBase58.decode(invalidStr))
      intercept[IllegalArgumentException](Base58.decode(invalidStr))
    }
  }
}

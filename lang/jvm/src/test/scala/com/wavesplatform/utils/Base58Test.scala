package com.wavesplatform.utils

import com.wavesplatform.common.utils.{Base58, FastBase58, StdBase58}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import scorex.crypto.encode.{Base58 => ScorexBase58}

import scala.util.Success

class Base58Test extends PropSpec with PropertyChecks with Matchers {
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

  property("works the same as scorex implementation") {
    forAll(base58Gen) { s =>
      val bytes       = StdBase58.decode(s)
      val scorexBytes = ScorexBase58.decode(s).get
      bytes.sameElements(scorexBytes) shouldBe true

      val str       = StdBase58.encode(bytes)
      val scorexStr = ScorexBase58.encode(bytes)
      str shouldBe scorexStr
    }
  }

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
    val encodedString  = "11WH5tQgZH6Djm7RS2guC"
    val Success(bytes) = ScorexBase58.decode(encodedString)

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

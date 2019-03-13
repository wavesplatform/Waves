package com.wavesplatform.utils

import com.wavesplatform.common.utils.Base64
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class Base64Test extends PropSpec with PropertyChecks with Matchers {

  private val Base64Chars  = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/"
  private val IllegalChars = "!@#$%^&*()_-?/.,<>|\';:`~"

  val illegalGen: Gen[String] =
    for {
      length <- Gen.chooseNum(100, 1024)
      chars <- Gen
        .listOfN(length, Gen.oneOf(Base64Chars ++ IllegalChars))
        .filter(_.toSet.intersect(IllegalChars.toSet).nonEmpty)
    } yield chars.mkString

  property("handles empty sequences") {
    Base64.encode(Array.emptyByteArray) shouldBe ""
    Base64.decode("").get.length shouldBe 0
    Base64.decode("base64:").get.length shouldBe 0
  }

  property("decoding fails on illegal characters") {
    forAll(illegalGen) { s =>
      Base64.decode(s).isSuccess shouldBe false
    }
  }

  property("decoding fails on null") {
    Base64.decode(null).isSuccess shouldBe false
  }
}

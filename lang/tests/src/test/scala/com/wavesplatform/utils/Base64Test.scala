package com.wavesplatform.utils

import com.wavesplatform.common.utils.Base64
import com.wavesplatform.test.PropSpec
import org.scalacheck.Gen

class Base64Test extends PropSpec {

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
    Base64.tryDecode("").get.length shouldBe 0
    Base64.tryDecode("base64:").get.length shouldBe 0
  }

  property("decoding fails on illegal characters") {
    forAll(illegalGen) { s =>
      Base64.tryDecode(s).isSuccess shouldBe false
    }
  }

  property("decoding fails on null") {
    Base64.tryDecode(null).isSuccess shouldBe false
  }
}

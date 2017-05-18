package com.wavesplatform

import org.scalatest.{FlatSpec, Matchers}
import scorex.app.ApplicationVersion

class ApplicationVersionSpecification extends FlatSpec with Matchers {
  "ApplicationVersion" should "be compatible with other versions" in {
    ApplicationVersion(0, 2, 0).compatibleWith(ApplicationVersion(0, 2, 0)) should be(true)
    ApplicationVersion(0, 2, 0).compatibleWith(ApplicationVersion(0, 2, 2)) should be(true)
    ApplicationVersion(0, 2, 0).compatibleWith(ApplicationVersion(0, 1, 3)) should be(true)
    ApplicationVersion(0, 2, 0).compatibleWith(ApplicationVersion(0, 3, 3)) should be(true)

    ApplicationVersion(1, 0, 0).compatibleWith(ApplicationVersion(1, 0, 0)) should be(true)
    ApplicationVersion(1, 0, 0).compatibleWith(ApplicationVersion(1, 0, 1)) should be(true)
    ApplicationVersion(1, 0, 0).compatibleWith(ApplicationVersion(1, 1, 3)) should be(true)
  }

  it should "be incompatible with other versions" in {
    ApplicationVersion(0, 2, 0).compatibleWith(ApplicationVersion(0, 0, 0)) should be(false)
    ApplicationVersion(0, 2, 0).compatibleWith(ApplicationVersion(0, 0, 2)) should be(false)
    ApplicationVersion(0, 2, 0).compatibleWith(ApplicationVersion(0, 4, 0)) should be(false)
    ApplicationVersion(0, 2, 0).compatibleWith(ApplicationVersion(0, 4, 3)) should be(false)
    ApplicationVersion(0, 2, 0).compatibleWith(ApplicationVersion(1, 0, 0)) should be(false)
    ApplicationVersion(0, 2, 0).compatibleWith(ApplicationVersion(1, 4, 3)) should be(false)

    ApplicationVersion(1, 0, 0).compatibleWith(ApplicationVersion(0, 8, 0)) should be(false)
    ApplicationVersion(1, 0, 0).compatibleWith(ApplicationVersion(0, 8, 8)) should be(false)
    ApplicationVersion(1, 0, 0).compatibleWith(ApplicationVersion(1, 2, 0)) should be(false)
    ApplicationVersion(1, 0, 0).compatibleWith(ApplicationVersion(1, 2, 3)) should be(false)
    ApplicationVersion(1, 0, 0).compatibleWith(ApplicationVersion(2, 0, 0)) should be(false)
    ApplicationVersion(1, 0, 0).compatibleWith(ApplicationVersion(2, 0, 8)) should be(false)

    ApplicationVersion(1, 0, 0).compatibleWith(ApplicationVersion(0, 9, 0)) should be(false)
  }

  it should "be converted to string correctly" in {
    ApplicationVersion(0, 0, 0).toString should be("0.0.0")
    ApplicationVersion(0, 1, 2).toString should be("0.1.2")
    ApplicationVersion(10, 100, 1000).toString should be("10.100.1000")
    ApplicationVersion(123, 456, 789).toString should be("123.456.789")
    ApplicationVersion(0, 13, 19).toString should be("0.13.19")
  }
}

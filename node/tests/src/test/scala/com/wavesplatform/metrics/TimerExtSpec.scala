package com.wavesplatform.metrics

import kamon.Kamon
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TimerExtSpec extends AnyFreeSpec with Matchers {
  "TimerExt" - {
    "measure executes function and returns result" in {
      val timer = Kamon.timer("test.timer.measure").withoutTags()
      val result = timer.measure {
        42
      }
      result shouldBe 42
    }

    "measureSuccessful records only on Right results" in {
      val timer = Kamon.timer("test.timer.successful").withoutTags()
      val rightResult = timer.measureSuccessful {
        Right("ok")
      }
      rightResult shouldBe Right("ok")

      val leftResult = timer.measureSuccessful {
        Left("error")
      }
      leftResult shouldBe Left("error")
    }

    "measureOptional records only on Some results" in {
      val timer = Kamon.timer("test.timer.optional").withoutTags()
      val someResult = timer.measureOptional {
        Some("value")
      }
      someResult shouldBe Some("value")

      val noneResult = timer.measureOptional {
        None
      }
      noneResult shouldBe None
    }
  }
}

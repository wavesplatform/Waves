package com.wavesplatform.state

import cats.syntax.option.*
import com.wavesplatform.test.*

class GenerationPeriodSpec extends FreeSpec {
  private val defaultActivation             = 7
  private val defaultGenerationPeriodLength = 3

  "from" - {
    "returns None if feature not activated" in {
      GenerationPeriod.from(Height(5), Height(7), 3) shouldBe empty
    }

    "start is expected" - {
      "different h" in {
        forAll(
          Table(
            ("title", "h", "expectedStart"),
            ("Zero period", 7, 7),
            ("Zero period on second block", 8, 7),
            ("Zero period includes last block", 10, 7),
            ("First period", 11, 11),
            ("First period on second block", 12, 11),
            ("First period includes last block", 13, 11),
            ("Second period", 14, 14)
          )
        ) { (_, h, expectedStart) =>
          generationPeriodFrom(h).start shouldBe Height(expectedStart)
        }
      }

      "another activation height" in {
        generationPeriodFrom(h = 1, activation = 0).start shouldBe Height(0)
        generationPeriodFrom(h = 4, activation = 0).start shouldBe Height(4)
        generationPeriodFrom(h = 100, activation = 100).start shouldBe Height(100)
      }

      "another generation period length" in {
        generationPeriodFrom(h = 10, activation = 10, len = 100).start shouldBe Height(10)
      }
    }
  }

  "end" in {
    generationPeriodOf(start = 7, activation = 7, len = 3).end shouldBe Height(10)
    generationPeriodOf(start = 11, activation = 7, len = 3).end shouldBe Height(13)
  }

  "next" in forAll(
    Table(
      ("title", "start", "expectedStart"),
      ("Zero period", 7, 11),
      ("First period", 11, 14)
    )
  ) { (_, start, expectedStart) =>
    generationPeriodOf(start).next shouldBe generationPeriodOf(expectedStart)
  }

  "prev" in forAll(
    Table(
      ("title", "start", "expectedStart"),
      ("Zero period", 7, none),
      ("First period", 11, 7.some),
      ("Second period", 14, 11.some)
    )
  ) { (_, start, expectedStart) =>
    generationPeriodOf(start).prev.map(_.start) shouldBe expectedStart
  }

  "enclosedPeriods" in forAll(
    Table(
      ("title", "start", "end", "expected"),
      ("All before activation", defaultActivation - 3, defaultActivation - 2, none),
      (
        "Before and on activation",
        defaultActivation - 3,
        defaultActivation,
        (zeroPeriod(), generationPeriodN(1)).some
      ),
      (
        "Before and after activation",
        defaultActivation - 3,
        defaultActivation + 1,
        (zeroPeriod(), generationPeriodN(1)).some
      ),
      (
        "On activation",
        defaultActivation,
        defaultActivation,
        (zeroPeriod(), generationPeriodN(1)).some
      ),
      (
        "After activation on zero period",
        defaultActivation + 1,
        defaultActivation + 2,
        (zeroPeriod(), generationPeriodN(1)).some
      ),
      (
        "After activation on zero and first period",
        defaultActivation + 1,
        defaultActivation + defaultGenerationPeriodLength + 1,
        (zeroPeriod(), generationPeriodN(2)).some
      ),
      (
        "After activation on first and second period",
        defaultActivation + defaultGenerationPeriodLength + 1,
        defaultActivation + defaultGenerationPeriodLength * 2 + 1,
        (zeroPeriod(), generationPeriodN(3)).some
      ),
      (
        "Before activation and on second period",
        defaultActivation - 1,
        defaultActivation + defaultGenerationPeriodLength * 2 + 1,
        (zeroPeriod(), generationPeriodN(3)).some
      )
    )
  ) { (_, start, end, expected) =>
    enclosedPeriods(start, end) shouldBe expected
  }

  private def generationPeriodFrom(h: Int, activation: Int = defaultActivation, len: Int = defaultGenerationPeriodLength) =
    GenerationPeriod.from(Height(h), Height(activation), len).value

  private def generationPeriodOf(start: Int, activation: Int = defaultActivation, len: Int = defaultGenerationPeriodLength) =
    GenerationPeriod(Height(activation), Height(start), len)

  // "Zero", because can't commit on this period
  private def zeroPeriod(activation: Int = defaultActivation, generationPeriodLength: Int = defaultGenerationPeriodLength) =
    generationPeriodOf(activation, activation, generationPeriodLength)

  private def generationPeriodN(n: Int, activation: Int = defaultActivation, len: Int = defaultGenerationPeriodLength) =
    if (n == 0) zeroPeriod(activation, len)
    else generationPeriodOf(activation + n * len + 1, activation, len)

  private def enclosedPeriods(
      start: Int,
      end: Int,
      activation: Int = defaultActivation,
      len: Int = defaultGenerationPeriodLength
  ): Option[(start: GenerationPeriod, end: GenerationPeriod)] =
    GenerationPeriod.enclosedPeriods(Height(activation), len, Height(start), Height(end))
}

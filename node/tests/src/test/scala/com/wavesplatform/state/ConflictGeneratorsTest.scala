package com.wavesplatform.state

import com.wavesplatform.test.FreeSpec

class ConflictGeneratorsTest extends FreeSpec {
  "accumulates generators correctly with strictly increasing heights" in {
    val cg = ConflictGenerators.empty
      .appendAll(1, 0)
      .appendAll(3, idxs = 1, 2)

    cg.isEmpty shouldBe false

    cg.heightOf(0).value shouldBe Height(1)
    cg.heightOf(1).value shouldBe Height(3)
    cg.heightOf(2).value shouldBe Height(3)
    cg.heightOf(3) shouldBe None
  }

  "enforces strictly increasing heights" in {
    val cg = ConflictGenerators.empty.appendAll(5, idxs = 10)

    withClue("equal height not allowed: ") {
      val e = intercept[IllegalArgumentException] {
        cg.appendAll(h = 5, 11)
      }
      e.getMessage should include("must increase")
    }

    withClue("equal height not allowed: ") {
      val e = intercept[IllegalArgumentException] {
        cg.appendAll(h = 4, 12)
      }
      e.getMessage should include("must increase")
    }
  }

  "appendAll" - {
    "is a no-op for empty idxs" in {
      val cg1 = ConflictGenerators.empty.appendAll(1, idxs = 10)
      val cg2 = cg1.appendAll(2)

      cg2 shouldBe cg1
    }
  }

  "upTo" - {
    "returns empty set below smallest height" in {
      ConflictGenerators.empty.appendAll(10, idxs = 1).upTo(5) shouldBe Set.empty
    }

    "returns all generators from prefix up to given height" in {
      val cg = ConflictGenerators.empty
        .appendAll(2, idxs = 0, 1)
        .appendAll(5, 2)
        .appendAll(10, 3, 4)

      cg.upToRaw(1) shouldBe Set.empty
      val upToRaw2 = cg.upToRaw(2)
      upToRaw2 shouldBe Set(0, 1)
      cg.upToRaw(3) shouldBe upToRaw2

      cg.upToRaw(5) shouldBe Set(0, 1, 2)

      val upToRaw10 = cg.upToRaw(10)
      upToRaw10 shouldBe Set(0, 1, 2, 3, 4)
      cg.upToRaw(100) shouldBe upToRaw10
    }
  }

  "hasInUpTo" - {
    "checks membership consistently with upTo" in {
      val cg = ConflictGenerators.empty
        .appendAll(2, idxs = 0)
        .appendAll(4, 1)

      cg.hasInUpTo(1, idx = 0) shouldBe false
      cg.hasInUpTo(2, 0) shouldBe true
      cg.hasInUpTo(3, 1) shouldBe false
      cg.hasInUpTo(3, 99) shouldBe false
      cg.hasInUpTo(4, 1) shouldBe true
      cg.hasInUpTo(10, 99) shouldBe false
    }
  }

  "deleteLastIf" - {
    "removes last entry when height matches" in {
      val cg = ConflictGenerators.empty
        .appendAll(2, 1)
        .appendAll(4, 2)

      val deleted = cg.deleteLastIf(4)

      deleted.heightOf(1) shouldBe Some(2)
      deleted.heightOf(2) shouldBe None

      val unchanged = deleted.deleteLastIf(100)
      unchanged shouldBe deleted
    }

    "is a no-op on empty structure" in {
      val cg = ConflictGenerators.empty
      cg.deleteLastIf(1) shouldBe cg
    }
  }

  // Reduce noice in tests
  private given Conversion[Int, Height]         = (x: Int) => Height(x)
  private given Conversion[Int, GeneratorIndex] = (x: Int) => GeneratorIndex(x)

  extension (self: ConflictGenerators) {
    def upToRaw(h: Int): Set[Int] = self.upTo(h).map(_.toInt)
  }
}

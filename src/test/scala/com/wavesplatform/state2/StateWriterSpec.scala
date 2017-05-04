package com.wavesplatform.state2

import org.h2.mvstore.MVStore
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, Outcome, fixture}

class StateWriterSpec extends fixture.FunSuite with Matchers with GeneratorDrivenPropertyChecks {
  override type FixtureParam = StateStorage

  override protected def withFixture(test: OneArgTest): Outcome = {
    val storage = new StateStorage(new MVStore.Builder().open())
    test(storage)
  }

  test("increase height when applying block diff") { storage =>
    val writer = new StateWriterImpl(storage)
    forAll(Gen.choose(0, Int.MaxValue)) { heightDiff =>
      val h = writer.height
      writer.applyBlockDiff(BlockDiff(Diff.empty, heightDiff, Map.empty))
      writer.height shouldBe h + heightDiff
      storage.getHeight shouldBe h + heightDiff
    }
  }
}

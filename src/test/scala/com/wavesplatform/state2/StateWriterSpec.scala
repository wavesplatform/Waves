package com.wavesplatform.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import org.h2.mvstore.MVStore
import org.scalacheck.Gen
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class StateWriterSpec extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {
  test("increase height when applying block diff") {
    val storage = StateStorage(new MVStore.Builder().open()).explicitGet()
    val writer = new StateWriterImpl(storage, new ReentrantReadWriteLock())
    forAll(Gen.choose(0, Int.MaxValue)) { heightDiff =>
      val h = writer.height
      writer.applyBlockDiff(BlockDiff(Diff.empty, heightDiff, Map.empty))
      writer.height shouldBe h + heightDiff
      storage.getHeight shouldBe h + heightDiff
    }
  }
}

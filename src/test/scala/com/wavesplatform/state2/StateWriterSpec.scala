package com.wavesplatform.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.TestDB
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}

class StateWriterSpec extends FunSuite with TestDB with Matchers with GeneratorDrivenPropertyChecks {
  test("increase height when applying block diff") {
    val storage = StateStorage(open(), dropExisting = false).get
    val writer = new StateWriterImpl(storage, new ReentrantReadWriteLock())
    forAll(Gen.choose(0, Int.MaxValue)) { heightDiff =>
      val h = writer.height
      writer.applyBlockDiff(BlockDiff(Diff.empty, heightDiff, Map.empty))
      writer.height shouldBe h + heightDiff
      storage.getHeight shouldBe h + heightDiff
    }
  }
}

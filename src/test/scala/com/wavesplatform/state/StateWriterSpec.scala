package com.wavesplatform.state

import org.scalatest.{FunSuite, Matchers}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks => GeneratorDrivenPropertyChecks}

class StateWriterSpec extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {
  ignore("increase height when applying block diff") {
//    val storage = StateStorage(None, dropExisting = false).get
//    val writer = new StateWriterImpl(???)
//    forAll(Gen.choose(0, Int.MaxValue)) { heightDiff =>
//      val h = writer.height
//      writer.applyBlockDiff(BlockDiff(Diff.empty, heightDiff, Map.empty), ???, 0)
//      writer.height shouldBe h + heightDiff
//      storage.getHeight shouldBe h + heightDiff
//    }
  }
}

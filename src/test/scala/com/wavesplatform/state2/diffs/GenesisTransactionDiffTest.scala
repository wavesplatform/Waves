package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.state2._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock

class GenesisTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
  def nelMax[T](g: Gen[T], max: Int = 10): Gen[List[T]] = Gen.choose(1, max).flatMap(Gen.listOfN(_, g))

  property("fails if height != 1") {
    forAll(genesisGen, positiveIntGen suchThat (_ > 1)) { (gtx, h) =>
      GenesisTransactionDiff(h)(gtx) should produce("GenesisTransaction cannot appear in non-initial block")
    }
  }

  property("Diff establishes Waves invariant") {
    forAll(nelMax(genesisGen)) { gtxs =>
      assertDiffAndState(Seq.empty, TestBlock.create(gtxs)) { (blockDiff, state) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe gtxs.map(_.amount).sum
        totalPortfolioDiff.effectiveBalance shouldBe gtxs.map(_.amount).sum
        totalPortfolioDiff.assets shouldBe Map.empty

        gtxs.foreach { gtx =>
          blockDiff.snapshots(gtx.recipient) shouldBe Map(1 -> Snapshot(0, gtx.amount, gtx.amount))
        }
      }
    }
  }
}
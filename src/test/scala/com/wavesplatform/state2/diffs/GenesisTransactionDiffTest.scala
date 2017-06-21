package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock

class GenesisTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  def nelMax[T](g: Gen[T], max: Int = 10): Gen[List[T]] = Gen.choose(1, max).flatMap(Gen.listOfN(_, g))

  property("fails if height != 1") {
    forAll(genesisGen, positiveIntGen suchThat (_ > 1)) { (gtx, h) =>
      GenesisTransactionDiff(h)(gtx) should produce("GenesisTranaction cannot appear in non-initial block")
    }
  }

  property("Diff establishes Waves invariant") {
    forAll(nelMax(genesisGen)) { gtxs =>
      assertDiffAndState(Seq.empty, TestBlock(gtxs)) { (blockDiff, state) =>
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
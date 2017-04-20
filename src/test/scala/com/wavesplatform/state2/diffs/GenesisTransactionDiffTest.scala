package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock

class GenesisTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  def nelMax[T](g: Gen[T], max: Int = 10): Gen[List[T]] = Gen.choose(1, max).flatMap(Gen.listOfN(_, g))

  property("fails if height != 1") {
    forAll(genesisGen, positiveIntGen suchThat (_ > 1)) { (gtx, h) =>
      GenesisTransactionDiff(h)(gtx) should produce("GenesisTranaction cannot appear in non-initial block")
    }
  }

  property("Diff establishes Waves invariant") {
    forAll(nelMax(genesisGen), accountGen) { (gtxs, miner) =>
      assertDiffAndState(Seq.empty, TestBlock(gtxs, miner)) { (blockDiff, state) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe gtxs.map(_.amount).sum
        totalPortfolioDiff.effectiveBalance shouldBe gtxs.map(_.amount).sum
        totalPortfolioDiff.assets shouldBe Map.empty

        gtxs.foreach { gtx =>
          blockDiff.effectiveBalanceSnapshots.contains(EffectiveBalanceSnapshot(gtx.recipient, 1, gtx.amount, gtx.amount, gtx.amount))
        }
      }
    }
  }
}
package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.state2.{EffectiveBalanceSnapshot, Portfolio, portfolioMonoid}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.TransactionGen

class GenesisTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  property("fails if height != 1") {
    forAll(genesisGenerator, positiveIntGen suchThat (_ > 1)) { (gtx, h) =>
      GenesisTransactionDiff(h)(gtx) shouldBe 'left
    }
  }

  property("Diff establishes Waves invariant") {
    forAll(nelMax(genesisGenerator), accountGen) { (gtxs, miner) =>
      assertDiffAndState(Seq.empty, TestBlock(gtxs, miner)) { (blockDiff, state) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe gtxs.map(_.amount).sum
        totalPortfolioDiff.effectiveBalance shouldBe gtxs.map(_.amount).sum
        totalPortfolioDiff.assets shouldBe Map.empty

        gtxs.foreach { gtx =>
          blockDiff.effectiveBalanceSnapshots.contains(EffectiveBalanceSnapshot(gtx.recipient, 1, gtx.amount, gtx.amount))
        }
      }
    }
  }
}
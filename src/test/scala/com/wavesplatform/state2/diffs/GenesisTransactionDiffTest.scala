package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.state2.{Portfolio, portfolioMonoid}
import com.wavesplatform.state2.reader.{StateReader, StateReaderImpl}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{FreeSpec, Matchers}
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.TransactionGen

class GenesisTransactionDiffTest extends FreeSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  def newState: StateReader = new StateReaderImpl(new TestStorage)

  "GenesisTransactionDiff" - {
    "fails if height != 1" in {
      forAll(genesisGenerator, positiveIntGen suchThat (_ > 1)) { (gtx, h) =>
        GenesisTransactionDiff(h)(gtx) shouldBe 'left
      }
    }

    "establishes waves invariant" in {
      forAll(nelMax(genesisGenerator), accountGen) { (gtxs, miner) =>
        val totalDiff = BlockDiffer(TestFunctionalitySettings.Enabled)(newState, TestBlock(gtxs, miner)).right.get

        val totalPortfolioDiff: Portfolio = Monoid.combineAll(totalDiff.txsDiff.portfolios.values)

        totalPortfolioDiff.balance shouldBe gtxs.map(_.amount).sum
        totalPortfolioDiff.effectiveBalance shouldBe gtxs.map(_.amount).sum
        totalPortfolioDiff.assets shouldBe Map.empty
      }
    }


  }
}

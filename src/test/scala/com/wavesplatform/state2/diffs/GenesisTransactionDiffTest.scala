package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.state2.{Portfolio, portfolioMonoid}
import com.wavesplatform.state2.reader.{StateReader, StateReaderImpl}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{FreeSpec, Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.TransactionGen

class GenesisTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {


  property("fails if height != 1") {
    forAll(genesisGenerator, positiveIntGen suchThat (_ > 1)) { (gtx, h) =>
      GenesisTransactionDiff(h)(gtx) shouldBe 'left
    }
  }

  property("establishes waves invariant") {
    forAll(nelMax(genesisGenerator), accountGen) { (gtxs, miner) =>
      val totalDiff = BlockDiffer(TestFunctionalitySettings.Enabled)(new StateReaderImpl(new TestStorage), TestBlock(gtxs, miner)).right.get

      val totalPortfolioDiff: Portfolio = Monoid.combineAll(totalDiff.txsDiff.portfolios.values)

      totalPortfolioDiff.balance shouldBe gtxs.map(_.amount).sum
      totalPortfolioDiff.effectiveBalance shouldBe gtxs.map(_.amount).sum
      totalPortfolioDiff.assets shouldBe Map.empty
    }
  }

}

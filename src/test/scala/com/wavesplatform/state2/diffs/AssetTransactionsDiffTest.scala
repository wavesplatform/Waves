package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.state2.{AssetInfo, EffectiveBalanceSnapshot, EqByteArray, Portfolio, portfolioMonoid}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.{GenesisTransaction, Transaction, TransactionGen}
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction}

class AssetTransactionsDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val irb: Gen[(Long, Long, Long)] = for {
    i <- positiveLongGen
    r <- positiveLongGen
    b <- positiveLongGen.suchThat(x => x < i + r)
  } yield (i, r, b)

  val issueReissueBurnTxs: Gen[((GenesisTransaction, IssueTransaction), (ReissueTransaction, BurnTransaction))] = for {
    master <- accountGen
    ts <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    (ia, ra, ba) <- irb
    (issue, reissue, burn) <- issueReissueGeneratorP(ia, ra, ba, master) suchThat (_._1.reissuable)
  } yield ((genesis, issue), (reissue, burn))

  property("Issue+Reissue+Burn do not break waves invariant and updates state") {
    forAll(issueReissueBurnTxs, accountGen) { case (((gen, issue), (reissue, burn)), miner) =>
      assertDiffAndState(Seq(TestBlock(Seq(gen, issue))), TestBlock(Seq(reissue, burn), miner)) { case (blockDiff, newState) =>
        val totalPortfolioDiff = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)

        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe 0
        totalPortfolioDiff.assets shouldBe Map(EqByteArray(reissue.assetId) -> (reissue.quantity - burn.amount))

        val totalAssetVolume = issue.quantity + reissue.quantity - burn.amount
        newState.accountPortfolio(issue.sender).assets shouldBe Map(EqByteArray(reissue.assetId) -> totalAssetVolume)
        newState.assetInfo(EqByteArray(issue.id)) shouldBe Some(AssetInfo(reissue.reissuable, totalAssetVolume))
      }
    }
  }

  property("Cannot reissue non-existing alias") {

  }

  property("Cannot reissue non-owned alias") {

  }

  property("Cannot reissue non-reissuable alias") {

  }

  property("Cannot burn non-existing alias") {

  }

  property("Cannot burn non-owned alias") {

  }

}
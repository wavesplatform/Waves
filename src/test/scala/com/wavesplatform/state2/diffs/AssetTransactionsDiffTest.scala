package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction}

class AssetTransactionsDiffTest extends PropSpec
  with PropertyChecks with Matchers with TransactionGen with NoShrink {

  def issueReissueBurnTxs(isReissuable: Boolean): Gen[((GenesisTransaction, IssueTransaction), (ReissueTransaction, BurnTransaction))] = for {
    master <- accountGen
    ts <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    ia <- positiveLongGen
    ra <- positiveLongGen
    ba <- positiveLongGen.suchThat(x => x < ia + ra)
    (issue, reissue, burn) <- issueReissueBurnGeneratorP(ia, ra, ba, master) suchThat (_._1.reissuable == isReissuable)
  } yield ((genesis, issue), (reissue, burn))


  property("Issue+Reissue+Burn do not break waves invariant and updates state") {
    forAll(issueReissueBurnTxs(isReissuable = true)) { case (((gen, issue), (reissue, burn))) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(gen, issue))), TestBlock.create(Seq(reissue, burn))) { case (blockDiff, newState) =>
        val totalPortfolioDiff = Monoid.combineAll(blockDiff.portfolios.values)

        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe 0
        totalPortfolioDiff.assets shouldBe Map(reissue.assetId -> (reissue.quantity - burn.amount))

        val totalAssetVolume = issue.quantity + reissue.quantity - burn.amount
        newState.portfolio(issue.sender).assets shouldBe Map(reissue.assetId -> totalAssetVolume)
      }
    }
  }

  property("Cannot reissue/burn non-existing alias") {
    val setup: Gen[(GenesisTransaction, ReissueTransaction, BurnTransaction)] = for {
      master <- accountGen
      ts <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      reissue <- reissueGen
      burn <- burnGen
    } yield (genesis, reissue, burn)

    forAll(setup) { case ((gen, reissue, burn)) =>
      assertDiffEi(Seq(TestBlock.create(Seq(gen))), TestBlock.create(Seq(reissue))) { blockDiffEi =>
        blockDiffEi should produce("Referenced assetId not found")
      }
      assertDiffEi(Seq(TestBlock.create(Seq(gen))), TestBlock.create(Seq(burn))) { blockDiffEi =>
        blockDiffEi should produce("Referenced assetId not found")
      }
    }
  }

  property("Cannot reissue/burn non-owned alias") {
    val setup = for {
      ((gen, issue), (_, _)) <- issueReissueBurnTxs(isReissuable = true)
      other <- accountGen.suchThat(_ != issue.sender.toAddress)
      quantity <- positiveLongGen
      reissuable2 <- Arbitrary.arbitrary[Boolean]
      fee <- Gen.choose(1L, 2000000L)
      timestamp <- timestampGen
      reissue = ReissueTransaction.create(other, issue.assetId(), quantity, reissuable2, fee, timestamp).right.get
      burn = BurnTransaction.create(other, issue.assetId(), quantity, fee, timestamp).right.get
    } yield ((gen, issue), reissue, burn)

    forAll(setup) { case ((gen, issue), reissue, burn) =>
      assertDiffEi(Seq(TestBlock.create(Seq(gen, issue))), TestBlock.create(Seq(reissue))) { blockDiffEi =>
        blockDiffEi should produce("Asset was issued by another address")
      }
      assertDiffEi(Seq(TestBlock.create(Seq(gen, issue))), TestBlock.create(Seq(burn))) { blockDiffEi =>
        blockDiffEi should produce("Asset was issued by another address")
      }
    }
  }

  property("Cannot reissue non-reissuable alias") {
    forAll(issueReissueBurnTxs(isReissuable = false)) { case ((gen, issue), (reissue, _)) =>
      assertDiffEi(Seq(TestBlock.create(Seq(gen, issue))), TestBlock.create(Seq(reissue))) { blockDiffEi =>
        blockDiffEi should produce("Asset is not reissuable")
      }
    }
  }
}

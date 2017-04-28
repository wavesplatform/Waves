package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.encode.Base58
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, MakeAssetNameUniqueTransaction, ReissueTransaction}
import scorex.transaction.GenesisTransaction

class AssetTransactionsDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  def issueReissueBurnTxs(isReissuable: Boolean): Gen[((GenesisTransaction, IssueTransaction), (ReissueTransaction, BurnTransaction, MakeAssetNameUniqueTransaction))] = for {
    master <- accountGen
    ts <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    ia <- positiveLongGen
    ra <- positiveLongGen
    ba <- positiveLongGen.suchThat(x => x < ia + ra)
    (issue, reissue, burn, makeUnique) <- issueReissueBurnMakeUniqueGeneratorP(ia, ra, ba, master) suchThat (_._1.reissuable == isReissuable)
  } yield ((genesis, issue), (reissue, burn, makeUnique))

  def issuesAndMakeUniquesWithSameName: Gen[((GenesisTransaction, IssueTransaction, MakeAssetNameUniqueTransaction, IssueTransaction, MakeAssetNameUniqueTransaction))] = for {
    sender <- accountGen
    ts <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(sender, ENOUGH_AMT, ts).right.get
    (_, assetName, description, quantity, decimals, reissuable, iFee, timestamp) <- issueParamGen
  } yield {
    val issue = IssueTransaction.create(sender, assetName, description, quantity, decimals, reissuable, iFee, timestamp).right.get
    val makeUnique = MakeAssetNameUniqueTransaction.create(sender, issue.assetId, iFee, timestamp).right.get
    val issue2 = IssueTransaction.create(sender, assetName, description, quantity, decimals, reissuable, iFee, timestamp + 1).right.get
    val makeUnique2 = MakeAssetNameUniqueTransaction.create(sender, issue2.assetId, iFee, timestamp).right.get
    (genesis, issue, makeUnique, issue2, makeUnique2)
  }

  property("Issue+Reissue+Burn+MakeUnique do not break waves invariant and updates state") {
    forAll(issueReissueBurnTxs(isReissuable = true), accountGen) { case (((gen, issue), (reissue, burn, makeUnique)), miner) =>
      assertDiffAndState(Seq(TestBlock(Seq(gen, issue))), TestBlock(Seq(reissue, burn, makeUnique), miner)) { case (blockDiff, newState) =>
        val totalPortfolioDiff = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)

        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe 0
        totalPortfolioDiff.assets shouldBe Map(EqByteArray(reissue.assetId) -> (reissue.quantity - burn.amount))

        val totalAssetVolume = issue.quantity + reissue.quantity - burn.amount
        newState.accountPortfolio(issue.sender).assets shouldBe Map(EqByteArray(reissue.assetId) -> totalAssetVolume)
        newState.assetInfo(EqByteArray(issue.id)) shouldBe Some(AssetInfo(reissue.reissuable, totalAssetVolume))
        newState.getAssetIdByUniqueName(EqByteArray(issue.name)) shouldBe Some(EqByteArray(issue.id))
      }
    }
  }

  property("Cannot reissue/burn/make unique non-existing alias") {
    val setup: Gen[(GenesisTransaction, ReissueTransaction, BurnTransaction, MakeAssetNameUniqueTransaction)] = for {
      master <- accountGen
      ts <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      reissue <- reissueGen
      burn <- burnGen
      makeUnique <- makeAssetNameUniqueGen
    } yield (genesis, reissue, burn, makeUnique)

    forAll(setup) { case ((gen, reissue, burn, makeUnique)) =>
      assertDiffEi(Seq(TestBlock(Seq(gen))), TestBlock(Seq(reissue))) { blockDiffEi =>
        blockDiffEi should produce ("Referenced assetId not found")
      }
      assertDiffEi(Seq(TestBlock(Seq(gen))), TestBlock(Seq(burn))) { blockDiffEi =>
        blockDiffEi should produce("Referenced assetId not found")
      }
      assertDiffEi(Seq(TestBlock(Seq(gen))), TestBlock(Seq(makeUnique))) { blockDiffEi =>
        blockDiffEi should produce("Referenced assetId not found")
      }
    }
  }

  property("Cannot reissue/burn/make unique non-owned alias") {
    val setup = for {
      ((gen, issue), (_, _, _)) <- issueReissueBurnTxs(isReissuable = true)
      other <- accountGen.suchThat(_ != issue.sender.toAccount)
      quantity <- positiveLongGen
      reissuable2 <- Arbitrary.arbitrary[Boolean]
      fee <- Gen.choose(1L, 2000000L)
      timestamp <- timestampGen
      reissue = ReissueTransaction.create(other, issue.assetId, quantity, reissuable2, fee, timestamp).right.get
      burn = BurnTransaction.create(other, issue.assetId, quantity, fee, timestamp).right.get
      makeUnique = MakeAssetNameUniqueTransaction.create(other, issue.assetId, fee, timestamp).right.get
    } yield ((gen, issue), reissue, burn, makeUnique)

    forAll(setup) { case ((gen, issue), reissue, burn, makeUnique) =>
      assertDiffEi(Seq(TestBlock(Seq(gen, issue))), TestBlock(Seq(reissue))) { blockDiffEi =>
        blockDiffEi should produce("Asset was issued by other address")
      }
      assertDiffEi(Seq(TestBlock(Seq(gen, issue))), TestBlock(Seq(burn))) { blockDiffEi =>
        blockDiffEi should produce("Asset was issued by other address")
      }
      assertDiffEi(Seq(TestBlock(Seq(gen, issue))), TestBlock(Seq(makeUnique))) { blockDiffEi =>
        blockDiffEi should produce("Asset was issued by other address")
      }
    }
  }

  property("Cannot make unique asset with already busy name") {
    forAll(issuesAndMakeUniquesWithSameName) { case ((gen, issue, makeUnique, issue2, makeUnique2)) =>
      assertDiffEi(Seq(TestBlock(Seq(gen, issue, issue2, makeUnique))), TestBlock(Seq(makeUnique2))) { blockDiffEi =>
        blockDiffEi should produce(s"Asset name has been verified for ${Base58.encode(issue.id)}")
      }
    }
  }

  property("Cannot reissue non-reissuable alias") {
    forAll(issueReissueBurnTxs(isReissuable = false)) { case ((gen, issue), (reissue, _, _)) =>
      assertDiffEi(Seq(TestBlock(Seq(gen, issue))), TestBlock(Seq(reissue))) { blockDiffEi =>
        blockDiffEi should produce("Asset is not reissuable")
      }
    }
  }
}

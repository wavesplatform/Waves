package com.wavesplatform.state2.diffs

import com.wavesplatform.TransactionGen
import com.wavesplatform.features.BlockchainFeatures
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock.{create => block}
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.GenesisTransaction
import com.wavesplatform.state2._

class SponsorshipDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("work") {
    val settings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.FeeSponsorship.id -> 0))
    val setup = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      (issueTx, sponsorTx, sponsor1Tx, cancelTx) <- sponsorFeeCancelSponsorFeeGen(master)
    } yield (genesis, issueTx, sponsorTx, sponsor1Tx, cancelTx)

    forAll(setup) {
      case (genesis, issue, sponsor, sponsor1, cancel) =>
        val setupBlocks = Seq(block(Seq(genesis, issue)))
        assertDiffAndState(setupBlocks, block(Seq(sponsor)), settings) {
          case (diff, state) =>
            diff.sponsorship shouldBe Map(sponsor.assetId -> SponsorshipValue(sponsor.minFee))
            state.assetDescription(sponsor.assetId).map(_.sponsorship) shouldBe Some(sponsor.minFee)
        }
        assertDiffAndState(setupBlocks, block(Seq(sponsor, sponsor1)), settings) {
          case (diff, state) =>
            diff.sponsorship shouldBe Map(sponsor.assetId -> SponsorshipValue(sponsor1.minFee))
            state.assetDescription(sponsor.assetId).map(_.sponsorship) shouldBe Some(sponsor1.minFee)
        }
        assertDiffAndState(setupBlocks, block(Seq(sponsor, sponsor1, cancel)), settings) {
          case (diff, state) =>
            diff.sponsorship shouldBe Map(sponsor.assetId -> SponsorshipValue(0))
            state.assetDescription(sponsor.assetId).map(_.sponsorship) shouldBe Some(0)
        }
    }
  }

  property("validation fails if asset don't exist") {
    val settings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.FeeSponsorship.id -> 0))
    val setup = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      (_, sponsorTx, _, cancelTx) <- sponsorFeeCancelSponsorFeeGen(master)
    } yield (genesis, sponsorTx, cancelTx)

    forAll(setup) {
      case (genesis, sponsor, cancel) =>
        val setupBlocks = Seq(block(Seq(genesis)))
        assertDiffEi(setupBlocks, block(Seq(sponsor)), settings) { blockDiffEi =>
          blockDiffEi should produce("Referenced assetId not found")
        }
        assertDiffEi(setupBlocks, block(Seq(cancel)), settings) { blockDiffEi =>
          blockDiffEi should produce("Referenced assetId not found")
        }
    }
  }

  property("validation fails prior to feature activation") {
    val settings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.FeeSponsorship.id -> 100))
    val setup = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      (issueTx, sponsorTx, _, cancelTx) <- sponsorFeeCancelSponsorFeeGen(master)
    } yield (genesis, issueTx, sponsorTx, cancelTx)

    forAll(setup) {
      case (genesis, issue, sponsor, cancel) =>
        val setupBlocks = Seq(block(Seq(genesis, issue)))
        assertDiffEi(setupBlocks, block(Seq(sponsor)), settings) { blockDiffEi =>
          blockDiffEi should produce("SponsorFeeTransaction transaction has not been activated")
        }
        assertDiffEi(setupBlocks, block(Seq(cancel)), settings) { blockDiffEi =>
          blockDiffEi should produce("CancelFeeSponsorshipTransaction transaction has not been activated")
        }
    }
  }
}

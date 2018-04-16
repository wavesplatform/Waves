package com.wavesplatform.state2.diffs

import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import com.wavesplatform.features.BlockchainFeatures
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock.{create => block}
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.GenesisTransaction

class SponsorshipDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  property("work") {
    val settings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.SponsoredFee.id -> 0))
    val setup = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      (issueTx, sponsorTx, cancelTx) <- sponsorFeeCancelSponsorFeeGen(master)
    } yield (genesis, issueTx, sponsorTx, cancelTx)

    forAll(setup) {
      case (genesis, issue, sponsor, cancel) =>
        val setupBlocks = Seq(block(Seq(genesis, issue)))
        assertDiffAndState(setupBlocks, block(Seq(sponsor)), settings) {
          case (diff, state) =>
            println(diff)
            println(state)
        }
        assertDiffAndState(setupBlocks, block(Seq(cancel)), settings) {
          case (diff, state) =>
            println(diff)
            println(state)
        }
    }
  }

  property("validation fails if asset don't exist") {
    val settings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.SponsoredFee.id -> 0))
    val setup = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      (_, sponsorTx, cancelTx) <- sponsorFeeCancelSponsorFeeGen(master)
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
    val settings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.SponsoredFee.id -> 100))
    val setup = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      (issueTx, sponsorTx, cancelTx) <- sponsorFeeCancelSponsorFeeGen(master)
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

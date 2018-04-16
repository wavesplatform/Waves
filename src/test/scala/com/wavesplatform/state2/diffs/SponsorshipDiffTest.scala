package com.wavesplatform.state2.diffs

import com.wavesplatform.TransactionGen
import com.wavesplatform.features.BlockchainFeatures
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock.{create => block}
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.GenesisTransaction

class SponsorshipDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("validation fails prior to feature activation") {
    val settings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.SponsoredFee.id -> 100))
    val setup = for {
      master <- accountGen
      ts     <- positiveLongGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      (issueTx, sponsorTx, cancelTx) <- sponsorFeeCancelSponsorFeeGen
    } yield (genesis, issueTx, sponsorTx, cancelTx)

    forAll(setup) {
      case (genesis, issue, sponsor, cancel) =>
        val setupBlocks = Seq(block(Seq(genesis)), block(Seq(issue)))
        assertDiffEi(setupBlocks, block(Seq(sponsor)), settings) { blockDiffEi =>
          blockDiffEi should produce("SponsorFeeTransaction transaction has not been activated")
        }
        assertDiffEi(setupBlocks, block(Seq(cancel)), settings) { blockDiffEi =>
          blockDiffEi should produce("CancelFeeSponsorshipTransaction transaction has not been activated")
        }
    }
  }
}

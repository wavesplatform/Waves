package com.wavesplatform.state.diffs

import com.wavesplatform.OldTransactionGen
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.encode.Base58
import scorex.lagonaki.mocks.TestBlock.{create => block}
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.GenesisTransaction
import scorex.transaction.assets.{IssueTransaction, TransferTransaction}
import scorex.transaction.modern.TxHeader
import scorex.transaction.modern.assets.{SponsorFeePayload, SponsorFeeTx}

class SponsorshipDiffTest extends PropSpec with PropertyChecks with Matchers with OldTransactionGen {

  def settings(sponsorshipActivationHeight: Int) =
    TestFunctionalitySettings.Enabled
      .copy(preActivatedFeatures = Map(BlockchainFeatures.FeeSponsorship.id -> sponsorshipActivationHeight), blocksForFeatureActivation = 1)

  property("work") {
    val s = settings(0)
    val setup = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      (issueTx, sponsorTx, sponsor1Tx, cancelTx) <- sponsorFeeCancelSponsorFeeGen(master)
    } yield (genesis, issueTx, sponsorTx, sponsor1Tx, cancelTx)

    forAll(setup) {
      case (genesis, issue, sponsor, sponsor1, cancel) =>
        val setupBlocks = Seq(block(Seq(genesis, issue)))
        assertDiffAndState(setupBlocks, block(Seq(sponsor)), s) {
          case (diff, state) =>
            diff.sponsorship shouldBe Map(sponsor.assetId -> SponsorshipValue(sponsor.minFee))
            state.assetDescription(sponsor.assetId).map(_.sponsorship) shouldBe Some(sponsor.minFee)
        }
        assertDiffAndState(setupBlocks, block(Seq(sponsor, sponsor1)), s) {
          case (diff, state) =>
            diff.sponsorship shouldBe Map(sponsor.assetId -> SponsorshipValue(sponsor1.minFee))
            state.assetDescription(sponsor.assetId).map(_.sponsorship) shouldBe Some(sponsor1.minFee)
        }
        assertDiffAndState(setupBlocks, block(Seq(sponsor, sponsor1, cancel)), s) {
          case (diff, state) =>
            diff.sponsorship shouldBe Map(sponsor.assetId -> SponsorshipValue(0))
            state.assetDescription(sponsor.assetId).map(_.sponsorship) shouldBe Some(0)
        }
    }
  }

  property("validation fails if asset doesn't exist") {
    val s = settings(0)
    val setup = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      (_, sponsorTx, _, cancelTx) <- sponsorFeeCancelSponsorFeeGen(master)
    } yield (genesis, sponsorTx, cancelTx)

    forAll(setup) {
      case (genesis, sponsor, cancel) =>
        val setupBlocks = Seq(block(Seq(genesis)))
        assertDiffEi(setupBlocks, block(Seq(sponsor)), s) { blockDiffEi =>
          blockDiffEi should produce("Referenced assetId not found")
        }
        assertDiffEi(setupBlocks, block(Seq(cancel)), s) { blockDiffEi =>
          blockDiffEi should produce("Referenced assetId not found")
        }
    }
  }

  property("validation fails prior to feature activation") {
    val s = settings(100)
    val setup = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      (issueTx, sponsorTx, _, cancelTx) <- sponsorFeeCancelSponsorFeeGen(master)
    } yield (genesis, issueTx, sponsorTx, cancelTx)

    forAll(setup) {
      case (genesis, issue, sponsor, cancel) =>
        val setupBlocks = Seq(block(Seq(genesis, issue)))
        assertDiffEi(setupBlocks, block(Seq(sponsor)), s) { blockDiffEi =>
          blockDiffEi should produce("SponsorFeeTx transaction has not been activated")
        }
        assertDiffEi(setupBlocks, block(Seq(cancel)), s) { blockDiffEi =>
          blockDiffEi should produce("CancelFeeSponsorshipTx transaction has not been activated")
        }
    }
  }

  property("not enough fee") {
    val s = settings(0)
    val setup = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, 400000000, ts).right.get
      (issueTx, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(master)
      recipient                  <- accountGen
      assetId = issueTx.id()
      assetOverspend = TransferTransaction
        .create(None, master, recipient.toAddress, 1000000, ts + 1, Some(assetId), issueTx.quantity + 1, Array.emptyByteArray)
        .right
        .get
      insufficientFee = TransferTransaction
        .create(None, master, recipient.toAddress, 1000000, ts + 2, Some(assetId), sponsorTx.minFee - 1, Array.emptyByteArray)
        .right
        .get
      fee = 3000 * sponsorTx.minFee
      wavesOverspend = TransferTransaction
        .create(None, master, recipient.toAddress, 1000000, ts + 3, Some(assetId), fee, Array.emptyByteArray)
        .right
        .get
    } yield (genesis, issueTx, sponsorTx, assetOverspend, insufficientFee, wavesOverspend)

    forAll(setup) {
      case (genesis, issue, sponsor, assetOverspend, insufficientFee, wavesOverspend) =>
        val setupBlocks = Seq(block(Seq(genesis, issue, sponsor)))
        assertDiffEi(setupBlocks, block(Seq(assetOverspend)), s) { blockDiffEi =>
          blockDiffEi should produce("unavailable funds")
        }
        assertDiffEi(setupBlocks, block(Seq(insufficientFee)), s) { blockDiffEi =>
          blockDiffEi should produce("does not exceed minimal value of 100000 WAVES")
        }
        assertDiffEi(setupBlocks, block(Seq(wavesOverspend)), s) { blockDiffEi =>
          if (wavesOverspend.fee > issue.quantity)
            blockDiffEi should produce("unavailable funds")
          else
            blockDiffEi should produce("negative waves balance")
        }
    }
  }

  property("sponsor has no WAVES but receives them just in time") {
    val s = settings(0)
    val setup = for {
      master    <- accountGen
      recipient <- accountGen
      ts        <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, 300000000, ts).right.get
      issue                       = IssueTransaction.create(master, Base58.decode("Asset").get, Array.emptyByteArray, 100, 2, false, 100000000, ts + 1).right.get
      assetId                     = issue.id()
      sponsor                     = SponsorFeeTx.selfSigned(TxHeader(SponsorFeeTx.typeId, 1, master, 100000000, ts + 2), SponsorFeePayload(assetId, 100)).get
      assetTransfer = TransferTransaction
        .create(Some(assetId), master, recipient, issue.quantity, ts + 3, None, 100000, Array.emptyByteArray)
        .right
        .get
      wavesTransfer = TransferTransaction
        .create(None, master, recipient, 99800000, ts + 4, None, 100000, Array.emptyByteArray)
        .right
        .get
      backWavesTransfer = TransferTransaction
        .create(None, recipient, master, 100000, ts + 5, Some(assetId), 100, Array.emptyByteArray)
        .right
        .get
    } yield (genesis, issue, sponsor, assetTransfer, wavesTransfer, backWavesTransfer)

    forAll(setup) {
      case (genesis, issue, sponsor, assetTransfer, wavesTransfer, backWavesTransfer) =>
        assertDiffAndState(Seq(block(Seq(genesis, issue, sponsor, assetTransfer, wavesTransfer))), block(Seq(backWavesTransfer)), s) {
          case (diff, state) =>
            val portfolio = state.portfolio(genesis.recipient)
            portfolio.balance shouldBe 0
            portfolio.assets(issue.id()) shouldBe issue.quantity
        }
    }
  }
}

package com.wavesplatform.state.diffs

import com.wavesplatform.TransactionGen
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock.{create => block}
import com.wavesplatform.settings.{Constants, FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.assets.{IssueTransactionV1, SponsorFeeTransaction}
import com.wavesplatform.transaction.lease.LeaseTransactionV1
import com.wavesplatform.transaction.transfer._
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class SponsorshipDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  def settings(sponsorshipActivationHeight: Int): FunctionalitySettings =
    TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.FeeSponsorship.id -> sponsorshipActivationHeight),
                                           featureCheckBlocksPeriod = 1,
                                           blocksForFeatureActivation = 1)

  property("work") {
    val s = settings(0)
    val setup = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      (issueTx, sponsorTx, sponsor1Tx, cancelTx) <- sponsorFeeCancelSponsorFeeGen(master)
    } yield (genesis, issueTx, sponsorTx, sponsor1Tx, cancelTx)

    forAll(setup) {
      case (genesis, issue, sponsor, sponsor1, cancel) =>
        val setupBlocks = Seq(block(Seq(genesis, issue)))
        assertDiffAndState(setupBlocks, block(Seq(sponsor)), s) {
          case (diff, state) =>
            diff.sponsorship shouldBe Map(sponsor.asset -> SponsorshipValue(sponsor.minSponsoredAssetFee.get))
            state.assetDescription(sponsor.asset).map(_.sponsorship) shouldBe sponsor.minSponsoredAssetFee
        }
        assertDiffAndState(setupBlocks, block(Seq(sponsor, sponsor1)), s) {
          case (diff, state) =>
            diff.sponsorship shouldBe Map(sponsor.asset -> SponsorshipValue(sponsor1.minSponsoredAssetFee.get))
            state.assetDescription(sponsor.asset).map(_.sponsorship) shouldBe sponsor1.minSponsoredAssetFee
        }
        assertDiffAndState(setupBlocks, block(Seq(sponsor, sponsor1, cancel)), s) {
          case (diff, state) =>
            diff.sponsorship shouldBe Map(sponsor.asset -> SponsorshipValue(0))
            state.assetDescription(sponsor.asset).map(_.sponsorship) shouldBe Some(0)
        }
    }
  }

  property("validation fails if asset doesn't exist") {
    val s = settings(0)
    val setup = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
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
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      (issueTx, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(master)
    } yield (genesis, issueTx, sponsorTx)

    forAll(setup) {
      case (genesis, issue, sponsor) =>
        val setupBlocks = Seq(block(Seq(genesis, issue)))
        assertDiffEi(setupBlocks, block(Seq(sponsor)), s) { blockDiffEi =>
          blockDiffEi should produce("Fee Sponsorship feature has not been activated yet")
        }
    }
  }

  property("not enough fee") {
    val s = settings(0)
    val setup = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, 400000000, ts).explicitGet()
      (issueTx, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(master)
      recipient                  <- accountGen
      assetId = issueTx.id()
      assetOverspend = TransferTransactionV1
        .selfSigned(Waves, master, recipient.toAddress, 1000000, ts + 1, IssuedAsset(assetId), issueTx.quantity + 1, Array.emptyByteArray)
        .right
        .get
      insufficientFee = TransferTransactionV1
        .selfSigned(Waves,
                    master,
                    recipient.toAddress,
                    1000000,
                    ts + 2,
                    IssuedAsset(assetId),
                    sponsorTx.minSponsoredAssetFee.get - 1,
                    Array.emptyByteArray)
        .right
        .get
      fee = 3000 * sponsorTx.minSponsoredAssetFee.get
      wavesOverspend = TransferTransactionV1
        .selfSigned(Waves, master, recipient.toAddress, 1000000, ts + 3, IssuedAsset(assetId), fee, Array.emptyByteArray)
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
          val minFee = Sponsorship
            .fromWaves(
              FeeValidation.FeeConstants(insufficientFee.builder.typeId) * FeeValidation.FeeUnit,
              sponsor.minSponsoredAssetFee.get
            )

          val expectedError =
            s"Fee for TransferTransaction (${insufficientFee.fee} in ${issue.assetId.base58})" ++
              s" does not exceed minimal value of 100000 WAVES or $minFee ${issue.assetId.base58}"

          blockDiffEi should produce(expectedError)
        }
        assertDiffEi(setupBlocks, block(Seq(wavesOverspend)), s) { blockDiffEi =>
          if (wavesOverspend.fee > issue.quantity)
            blockDiffEi should produce("unavailable funds")
          else
            blockDiffEi should produce("negative waves balance")
        }
    }
  }

  property("not enough waves to pay fee after leasing") {
    val s = settings(0)
    val setup = for {
      master <- accountGen
      alice  <- accountGen
      bob    <- accountGen
      ts     <- timestampGen
      fee    <- smallFeeGen
      amount                       = ENOUGH_AMT / 2
      genesis: GenesisTransaction  = GenesisTransaction.create(master, amount, ts).explicitGet()
      genesis2: GenesisTransaction = GenesisTransaction.create(bob, amount, ts).explicitGet()
      (issueTx, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(master)
      assetId = issueTx.id()
      transferAssetTx: TransferTransactionV1 = TransferTransactionV1
        .selfSigned(IssuedAsset(assetId), master, alice.toAddress, issueTx.quantity, ts + 2, Waves, fee, Array.emptyByteArray)
        .right
        .get
      leasingTx: LeaseTransactionV1 = LeaseTransactionV1
        .selfSigned(master, amount - issueTx.fee - sponsorTx.fee - 2 * fee, fee, ts + 3, bob)
        .right
        .get
      leasingToMasterTx: LeaseTransactionV1 = LeaseTransactionV1
        .selfSigned(bob, amount / 2, fee, ts + 3, master)
        .right
        .get
      insufficientFee = TransferTransactionV1
        .selfSigned(IssuedAsset(assetId),
                    alice,
                    bob.toAddress,
                    issueTx.quantity / 12,
                    ts + 4,
                    IssuedAsset(assetId),
                    sponsorTx.minSponsoredAssetFee.get,
                    Array.emptyByteArray)
        .right
        .get
    } yield (genesis, genesis2, issueTx, sponsorTx, transferAssetTx, leasingTx, insufficientFee, leasingToMasterTx)

    forAll(setup) {
      case (genesis, genesis2, issueTx, sponsorTx, transferAssetTx, leasingTx, insufficientFee, leasingToMaster) =>
        val setupBlocks = Seq(block(Seq(genesis, genesis2, issueTx, sponsorTx)), block(Seq(transferAssetTx, leasingTx)))
        assertDiffEi(setupBlocks, block(Seq(insufficientFee)), s) { blockDiffEi =>
          blockDiffEi should produce("negative effective balance")
        }
        assertDiffEi(setupBlocks, block(Seq(leasingToMaster, insufficientFee)), s) { blockDiffEi =>
          blockDiffEi should produce("trying to spend leased money")
        }
    }
  }

  property("cannot cancel sponsorship") {
    val s = settings(0)
    val setup = for {
      master     <- accountGen
      notSponsor <- accountGen
      ts         <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, 400000000, ts).explicitGet()
      (issueTx, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(master)
      assetId = IssuedAsset(issueTx.id())
      senderNotIssuer = SponsorFeeTransaction
        .selfSigned(notSponsor, assetId, None, 1 * Constants.UnitsInWave, ts + 1)
        .right
        .get
      insufficientFee = SponsorFeeTransaction
        .selfSigned(notSponsor, assetId, None, 1 * Constants.UnitsInWave - 1, ts + 1)
        .right
        .get
    } yield (genesis, issueTx, sponsorTx, senderNotIssuer, insufficientFee)

    forAll(setup) {
      case (genesis, issueTx, sponsorTx, senderNotIssuer, insufficientFee) =>
        val setupBlocks = Seq(block(Seq(genesis, issueTx, sponsorTx)))
        assertDiffEi(setupBlocks, block(Seq(senderNotIssuer)), s) { blockDiffEi =>
          blockDiffEi should produce("Asset was issued by other address")
        }
        assertDiffEi(setupBlocks, block(Seq(insufficientFee)), s) { blockDiffEi =>
          blockDiffEi should produce("(99999999 in WAVES) does not exceed minimal value of 100000000 WAVES")
        }
    }
  }

  property("cannot сhange sponsorship fee") {
    val s = settings(0)
    val setup = for {
      master     <- accountGen
      notSponsor <- accountGen
      ts         <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, 400000000, ts).explicitGet()
      (issueTx, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(master)
      assetId = IssuedAsset(issueTx.id())
      minFee <- smallFeeGen
      senderNotIssuer = SponsorFeeTransaction
        .selfSigned(notSponsor, assetId, Some(minFee), 1 * Constants.UnitsInWave, ts + 1)
        .right
        .get
      insufficientFee = SponsorFeeTransaction
        .selfSigned(master, assetId, Some(minFee), 1 * Constants.UnitsInWave - 1, ts + 1)
        .right
        .get
    } yield (genesis, issueTx, sponsorTx, senderNotIssuer, insufficientFee)

    forAll(setup) {
      case (genesis, issueTx, sponsorTx, senderNotIssuer, insufficientFee) =>
        val setupBlocks = Seq(block(Seq(genesis, issueTx, sponsorTx)))
        assertDiffEi(setupBlocks, block(Seq(senderNotIssuer)), s) { blockDiffEi =>
          blockDiffEi should produce("Asset was issued by other address")
        }
        assertDiffEi(setupBlocks, block(Seq(insufficientFee)), s) { blockDiffEi =>
          blockDiffEi should produce("(99999999 in WAVES) does not exceed minimal value of 100000000 WAVES")
        }
    }
  }

  property("sponsor has no WAVES but receives them just in time") {
    val s = settings(0)
    val setup = for {
      master    <- accountGen
      recipient <- accountGen
      ts        <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, 300000000, ts).explicitGet()
      issue = IssueTransactionV1
        .selfSigned(master, Base58.tryDecodeWithLimit("Asset").get, Array.emptyByteArray, 100, 2, reissuable = false, 100000000, ts + 1)
        .explicitGet()
      assetId = IssuedAsset(issue.id())
      sponsor = SponsorFeeTransaction.selfSigned(master, assetId, Some(100), 100000000, ts + 2).explicitGet()
      assetTransfer = TransferTransactionV1
        .selfSigned(assetId, master, recipient, issue.quantity, ts + 3, Waves, 100000, Array.emptyByteArray)
        .right
        .get
      wavesTransfer = TransferTransactionV1
        .selfSigned(Waves, master, recipient, 99800000, ts + 4, Waves, 100000, Array.emptyByteArray)
        .right
        .get
      backWavesTransfer = TransferTransactionV1
        .selfSigned(Waves, recipient, master, 100000, ts + 5, assetId, 100, Array.emptyByteArray)
        .right
        .get
    } yield (genesis, issue, sponsor, assetTransfer, wavesTransfer, backWavesTransfer)

    forAll(setup) {
      case (genesis, issue, sponsor, assetTransfer, wavesTransfer, backWavesTransfer) =>
        assertDiffAndState(Seq(block(Seq(genesis, issue, sponsor, assetTransfer, wavesTransfer))), block(Seq(backWavesTransfer)), s) {
          case (_, state) =>
            val portfolio = state.portfolio(genesis.recipient)
            portfolio.balance shouldBe 0
            portfolio.assets(IssuedAsset(issue.id())) shouldBe issue.quantity
        }
    }
  }

}

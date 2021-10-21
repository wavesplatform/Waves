package com.wavesplatform.state.diffs

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.BlockV5
import com.wavesplatform.lagonaki.mocks.TestBlock.{create => block}
import com.wavesplatform.settings.{Constants, FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state._
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.{IssueTransaction, SponsorFeeTransaction}
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import com.wavesplatform.utils._

class SponsorshipDiffTest extends PropSpec with WithState {

  def settings(sponsorshipActivationHeight: Int): FunctionalitySettings =
    TestFunctionalitySettings.Enabled.copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.FeeSponsorship.id -> sponsorshipActivationHeight,
        BlockchainFeatures.BlockV5.id        -> 0
      ),
      featureCheckBlocksPeriod = 1,
      blocksForFeatureActivation = 1
    )

  property("work") {
    val s = settings(0)
    val setup = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
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
      genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
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
      genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
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
      genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, 400000000, ts).explicitGet()
      (issueTx, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(master)
      recipient                  <- accountGen
      assetId = issueTx.id()
      assetOverspend = TransferTransaction.selfSigned(1.toByte, master, recipient.toAddress, Waves, 1000000, IssuedAsset(assetId), issueTx.quantity + 1, ByteStr.empty,  ts + 1)
        .explicitGet()
      insufficientFee = TransferTransaction.selfSigned(
          1.toByte,
          master,
          recipient.toAddress,
          Waves,
          1000000,
          IssuedAsset(assetId),
          sponsorTx.minSponsoredAssetFee.get - 1, ByteStr.empty,
          ts + 2
        )
        .explicitGet()
      fee = 3000 * sponsorTx.minSponsoredAssetFee.get
      wavesOverspend = TransferTransaction.selfSigned(1.toByte, master, recipient.toAddress, Waves, 1000000, IssuedAsset(assetId), fee, ByteStr.empty,  ts + 3)
        .explicitGet()
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
              FeeValidation.FeeConstants(insufficientFee.tpe) * FeeValidation.FeeUnit,
              sponsor.minSponsoredAssetFee.get
            )

          val expectedError =
            s"Fee for TransferTransaction (${insufficientFee.fee} in ${issue.assetId.toString})" ++
              s" does not exceed minimal value of 100000 WAVES or $minFee ${issue.assetId.toString}"

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
      genesis: GenesisTransaction  = GenesisTransaction.create(master.toAddress, amount, ts).explicitGet()
      genesis2: GenesisTransaction = GenesisTransaction.create(bob.toAddress, amount, ts).explicitGet()
      (issueTx, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(master)
      assetId = issueTx.id()
      transferAssetTx: TransferTransaction = TransferTransaction
        .selfSigned(1.toByte, master, alice.toAddress, IssuedAsset(assetId), issueTx.quantity, Waves, fee, ByteStr.empty,  ts + 2)
        .explicitGet()
      leasingTx = LeaseTransaction
        .selfSigned(1.toByte, master, bob.toAddress, amount - issueTx.fee - sponsorTx.fee - 2 * fee, fee, ts + 3)
        .explicitGet()
      leasingToMasterTx = LeaseTransaction
        .selfSigned(1.toByte, bob, master.toAddress, amount / 2, fee, ts + 3)
        .explicitGet()
      insufficientFee = TransferTransaction.selfSigned(
          1.toByte,
          alice,
          bob.toAddress,
          IssuedAsset(assetId),
          issueTx.quantity / 12,
          IssuedAsset(assetId),
          sponsorTx.minSponsoredAssetFee.get, ByteStr.empty,
          ts + 4
        )
        .explicitGet()
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
      genesis1: GenesisTransaction = GenesisTransaction.create(master.toAddress, 400000000, ts).explicitGet()
      genesis2: GenesisTransaction = GenesisTransaction.create(notSponsor.toAddress, 400000000, ts).explicitGet()
      (issueTx, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(master)
      assetId = IssuedAsset(issueTx.id())
      senderNotIssuer = SponsorFeeTransaction
        .selfSigned(1.toByte, notSponsor, assetId, None, 1 * Constants.UnitsInWave, ts + 1)
        .explicitGet()
      insufficientFee = SponsorFeeTransaction
        .selfSigned(1.toByte, notSponsor, assetId, None, 1 * Constants.UnitsInWave - 1, ts + 1)
        .explicitGet()
      insufficientReducedFee = SponsorFeeTransaction
        .selfSigned(1.toByte, notSponsor, assetId, None, (0.001 * Constants.UnitsInWave).toLong - 1, ts + 1)
        .explicitGet()
    } yield (Seq(genesis1, genesis2, issueTx, sponsorTx), senderNotIssuer, insufficientFee, insufficientReducedFee)

    forAll(setup) {
      case (preconditions, senderNotIssuer, _, insufficientReducedFee) =>
        val setupBlocks = Seq(block(preconditions), block(Seq()))
        assertDiffEi(setupBlocks, block(Seq(senderNotIssuer)), s) { blockDiffEi =>
          blockDiffEi should produce("Asset was issued by other address")
        }
        assertDiffEi(setupBlocks, block(Seq(insufficientReducedFee)), s) { blockDiffEi =>
          blockDiffEi should produce("(99999 in WAVES) does not exceed minimal value of 100000 WAVES")
        }
    }
  }

  property("cannot change sponsorship fee") {
    val s = settings(0)
    val setup = for {
      master     <- accountGen
      notSponsor <- accountGen
      ts         <- timestampGen
      genesis1: GenesisTransaction = GenesisTransaction.create(master.toAddress, 400000000, ts).explicitGet()
      genesis2: GenesisTransaction = GenesisTransaction.create(notSponsor.toAddress, 400000000, ts).explicitGet()
      (issueTx, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(master)
      assetId = IssuedAsset(issueTx.id())
      minFee <- smallFeeGen
      senderNotIssuer = SponsorFeeTransaction
        .selfSigned(1.toByte, notSponsor, assetId, Some(minFee), 1 * Constants.UnitsInWave, ts + 1)
        .explicitGet()
      insufficientFee = SponsorFeeTransaction
        .selfSigned(1.toByte, master, assetId, Some(minFee), (0.001 * Constants.UnitsInWave).toLong - 1, ts + 1)
        .explicitGet()
    } yield (Seq(genesis1, genesis2, issueTx, sponsorTx), senderNotIssuer, insufficientFee)

    forAll(setup) {
      case (preconditions, senderNotIssuer, insufficientFee) =>
        val setupBlocks = Seq(block(preconditions))
        assertDiffEi(setupBlocks, block(Seq(senderNotIssuer)), s) { blockDiffEi =>
          blockDiffEi should produce("Asset was issued by other address")
        }
        assertDiffEi(setupBlocks, block(Seq(insufficientFee)), s) { blockDiffEi =>
          blockDiffEi should produce("(99999 in WAVES) does not exceed minimal value of 100000 WAVES")
        }
    }
  }

  property(s"sponsor has no WAVES but receives them just in time before $BlockV5 activation") {
    val s = settings(0)
    val setup = for {
      master    <- accountGen
      recipient <- accountGen
      ts        <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, 300000000, ts).explicitGet()
      issue = IssueTransaction(
        TxVersion.V1,
        master.publicKey,
        "Asset".utf8Bytes,
        Array.emptyByteArray,
        100,
        2,
        reissuable = false,
        script = None,
        100000000,
        ts + 1
      ).signWith(master.privateKey)
      assetId = IssuedAsset(issue.id())
      sponsor = SponsorFeeTransaction.selfSigned(1.toByte, master, assetId, Some(100), 100000000, ts + 2).explicitGet()
      assetTransfer = TransferTransaction.selfSigned(1.toByte, master, recipient.toAddress, assetId, issue.quantity, Waves, 100000, ByteStr.empty,  ts + 3)
        .explicitGet()
      wavesTransfer = TransferTransaction.selfSigned(1.toByte, master, recipient.toAddress, Waves, 99800000, Waves, 100000, ByteStr.empty,  ts + 4)
        .explicitGet()
      backWavesTransfer = TransferTransaction
        .selfSigned(1.toByte, recipient, master.toAddress, Waves, 100000, assetId, 100, ByteStr.empty,  ts + 5)
        .explicitGet()
    } yield (genesis, issue, sponsor, assetTransfer, wavesTransfer, backWavesTransfer)

    forAll(setup) {
      case (genesis, issue, sponsor, assetTransfer, wavesTransfer, backWavesTransfer) =>
        assertDiffAndState(
          Seq(block(Seq(genesis, issue, sponsor, assetTransfer, wavesTransfer))),
          block(Seq(backWavesTransfer)),
          s.copy(preActivatedFeatures = s.preActivatedFeatures + (BlockV5.id -> Int.MaxValue))
        ) {
          case (_, state) =>
            state.balance(genesis.recipient) shouldBe 0
            state.balance(genesis.recipient, IssuedAsset(issue.id())) shouldBe issue.quantity
        }

        assertDiffEi(
          Seq(block(Seq(genesis, issue, sponsor, assetTransfer, wavesTransfer))),
          block(Seq(backWavesTransfer)),
          s
        ) { ei =>
          ei should produce("negative waves balance")
        }
    }
  }
}

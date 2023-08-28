package com.wavesplatform.state.diffs

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.BlockV5
import com.wavesplatform.lagonaki.mocks.TestBlock.create as block
import com.wavesplatform.settings.{Constants, FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state.*
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{TxHelpers, TxVersion}

class SponsorshipDiffTest extends PropSpec with WithState {

  def settings(sponsorshipActivationHeight: Int): FunctionalitySettings =
    TestFunctionalitySettings.Enabled.copy(
      featureCheckBlocksPeriod = 1,
      blocksForFeatureActivation = 1,
      preActivatedFeatures = Map(
        BlockchainFeatures.FeeSponsorship.id -> sponsorshipActivationHeight,
        BlockchainFeatures.BlockV5.id        -> 0
      )
    )

  property("work") {
    val s      = settings(0)
    val master = TxHelpers.signer(1)

    val sponsorTxFee = (0.001 * Constants.UnitsInWave).toLong

    val genesis     = TxHelpers.genesis(master.toAddress)
    val issue       = TxHelpers.issue(master, version = TxVersion.V1)
    val sponsor     = TxHelpers.sponsor(issue.asset, Some(400000), master, fee = sponsorTxFee)
    val sponsor1    = TxHelpers.sponsor(issue.asset, Some(400001), master, fee = sponsorTxFee)
    val cancel      = TxHelpers.sponsor(issue.asset, None, master, fee = sponsorTxFee)
    val setupBlocks = Seq(block(Seq(genesis, issue)))

    assertDiffAndState(setupBlocks, block(Seq(sponsor)), s) { case (diff, state) =>
      diff.sponsorship shouldBe Map(sponsor.asset -> SponsorshipValue(sponsor.minSponsoredAssetFee.get.value))
      state.assetDescription(sponsor.asset).map(_.sponsorship) shouldBe sponsor.minSponsoredAssetFee.map(_.value)
    }
    assertDiffAndState(setupBlocks, block(Seq(sponsor, sponsor1)), s) { case (diff, state) =>
      diff.sponsorship shouldBe Map(sponsor.asset -> SponsorshipValue(sponsor1.minSponsoredAssetFee.get.value))
      state.assetDescription(sponsor.asset).map(_.sponsorship) shouldBe sponsor1.minSponsoredAssetFee.map(_.value)
    }
    assertDiffAndState(setupBlocks, block(Seq(sponsor, sponsor1, cancel)), s) { case (diff, state) =>
      diff.sponsorship shouldBe Map(sponsor.asset -> SponsorshipValue(0))
      state.assetDescription(sponsor.asset).map(_.sponsorship) shouldBe Some(0)
    }
  }

  property("validation fails if asset doesn't exist") {
    val s = settings(0)
    val setup = {
      val master = TxHelpers.signer(1)

      val asset        = IssuedAsset(ByteStr.fill(32)(1))
      val sponsorTxFee = (0.001 * Constants.UnitsInWave).toLong

      val genesis = TxHelpers.genesis(master.toAddress)
      val sponsor = TxHelpers.sponsor(asset, Some(400000), master, fee = sponsorTxFee)
      val cancel  = TxHelpers.sponsor(asset, None, master, fee = sponsorTxFee)

      (genesis, sponsor, cancel)
    }

    val (genesis, sponsor, cancel) = setup
    val setupBlocks                = Seq(block(Seq(genesis)))
    assertDiffEi(setupBlocks, block(Seq(sponsor)), s) { blockDiffEi =>
      blockDiffEi should produce("Referenced assetId not found")
    }
    assertDiffEi(setupBlocks, block(Seq(cancel)), s) { blockDiffEi =>
      blockDiffEi should produce("Referenced assetId not found")
    }
  }

  property("validation fails prior to feature activation") {
    val s = settings(100)
    val setup = {
      val master = TxHelpers.signer(1)

      val sponsorTxFee = (0.001 * Constants.UnitsInWave).toLong

      val genesis = TxHelpers.genesis(master.toAddress)
      val issue   = TxHelpers.issue(master, version = TxVersion.V1)
      val sponsor = TxHelpers.sponsor(issue.asset, Some(400000), master, fee = sponsorTxFee)

      (genesis, issue, sponsor)
    }

    val (genesis, issue, sponsor) = setup
    val setupBlocks               = Seq(block(Seq(genesis, issue)))
    assertDiffEi(setupBlocks, block(Seq(sponsor)), s) { blockDiffEi =>
      blockDiffEi should produce("Fee Sponsorship feature has not been activated yet")
    }
  }

  property("not enough fee") {
    val s = settings(0)
    val setup = {
      val master    = TxHelpers.signer(1)
      val recipient = TxHelpers.signer(2)

      val sponsorTxFee = (0.001 * Constants.UnitsInWave).toLong

      val genesis = TxHelpers.genesis(master.toAddress, 400000000)
      val issue   = TxHelpers.issue(master, version = TxVersion.V1)
      val sponsor = TxHelpers.sponsor(issue.asset, Some(400000), master, fee = sponsorTxFee)
      val assetOverspend =
        TxHelpers.transfer(master, recipient.toAddress, 1000000, feeAsset = issue.asset, fee = issue.quantity.value + 1, version = TxVersion.V1)
      val insufficientFee = TxHelpers.transfer(
        master,
        recipient.toAddress,
        1000000,
        feeAsset = issue.asset,
        fee = sponsor.minSponsoredAssetFee.get.value - 1,
        version = TxVersion.V1
      )

      val fee            = 3000 * sponsor.minSponsoredAssetFee.get.value
      val wavesOverspend = TxHelpers.transfer(master, recipient.toAddress, 1000000, feeAsset = issue.asset, fee = fee, version = TxVersion.V1)

      (genesis, issue, sponsor, assetOverspend, insufficientFee, wavesOverspend)
    }

    val (genesis, issue, sponsor, assetOverspend, insufficientFee, wavesOverspend) = setup
    val setupBlocks                                                                = Seq(block(Seq(genesis, issue, sponsor)))
    assertDiffEi(setupBlocks, block(Seq(assetOverspend)), s) { blockDiffEi =>
      blockDiffEi should produce("unavailable funds")
    }
    assertDiffEi(setupBlocks, block(Seq(insufficientFee)), s) { blockDiffEi =>
      val minFee = Sponsorship
        .fromWaves(
          FeeValidation.FeeConstants(insufficientFee.tpe) * FeeValidation.FeeUnit,
          sponsor.minSponsoredAssetFee.get.value
        )

      val expectedError =
        s"Fee for TransferTransaction (${insufficientFee.fee} in ${issue.assetId.toString})" ++
          s" does not exceed minimal value of 100000 WAVES or $minFee ${issue.assetId.toString}"

      blockDiffEi should produce(expectedError)
    }
    assertDiffEi(setupBlocks, block(Seq(wavesOverspend)), s) { blockDiffEi =>
      if (wavesOverspend.fee.value > issue.quantity.value)
        blockDiffEi should produce("unavailable funds")
      else
        blockDiffEi should produce("negative waves balance")
    }
  }

  property("not enough waves to pay fee after leasing") {
    val s = settings(0)
    val setup = {
      val master = TxHelpers.signer(1)
      val alice  = TxHelpers.signer(2)
      val bob    = TxHelpers.signer(3)

      val fee          = 400000
      val amount       = ENOUGH_AMT / 2
      val sponsorTxFee = (0.001 * Constants.UnitsInWave).toLong

      val genesis       = Seq(master, bob).map(acc => TxHelpers.genesis(acc.toAddress, amount))
      val issue         = TxHelpers.issue(master, version = TxVersion.V1)
      val sponsor       = TxHelpers.sponsor(issue.asset, Some(1), master, fee = sponsorTxFee)
      val transferAsset = TxHelpers.transfer(master, alice.toAddress, issue.quantity.value, issue.asset, fee = fee, version = TxVersion.V1)
      val leasing = TxHelpers.lease(master, bob.toAddress, amount - issue.fee.value - sponsor.fee.value - 2 * fee, fee = fee, version = TxVersion.V1)
      val leasingToMaster = TxHelpers.lease(bob, master.toAddress, amount / 2, fee = fee, version = TxVersion.V1)
      val insufficientFee = TxHelpers.transfer(
        alice,
        bob.toAddress,
        issue.quantity.value / 12,
        issue.asset,
        feeAsset = issue.asset,
        fee = sponsor.minSponsoredAssetFee.get.value,
        version = TxVersion.V1
      )

      (genesis, issue, sponsor, transferAsset, leasing, insufficientFee, leasingToMaster)
    }

    val (genesis, issueTx, sponsorTx, transferAssetTx, leasingTx, insufficientFee, leasingToMaster) = setup
    val setupBlocks = Seq(block(genesis :+ issueTx :+ sponsorTx), block(Seq(transferAssetTx, leasingTx)))
    assertDiffEi(setupBlocks, block(Seq(insufficientFee)), s) { blockDiffEi =>
      blockDiffEi should produce("negative effective balance")
    }
    assertDiffEi(setupBlocks, block(Seq(leasingToMaster, insufficientFee)), s) { blockDiffEi =>
      blockDiffEi should produce("trying to spend leased money")
    }
  }

  property("cannot cancel sponsorship") {
    val s = settings(0)
    val setup = {
      val master     = TxHelpers.signer(1)
      val notSponsor = TxHelpers.signer(2)

      val sponsorTxFee = (0.001 * Constants.UnitsInWave).toLong

      val genesis                = Seq(master, notSponsor).map(acc => TxHelpers.genesis(acc.toAddress, 400000000))
      val issue                  = TxHelpers.issue(master, version = TxVersion.V1)
      val sponsor                = TxHelpers.sponsor(issue.asset, Some(1), master, fee = sponsorTxFee)
      val senderNotIssuer        = TxHelpers.sponsor(issue.asset, None, notSponsor, fee = 1 * Constants.UnitsInWave)
      val insufficientFee        = TxHelpers.sponsor(issue.asset, None, notSponsor, fee = 1 * Constants.UnitsInWave - 1)
      val insufficientReducedFee = TxHelpers.sponsor(issue.asset, None, notSponsor, fee = (0.001 * Constants.UnitsInWave).toLong - 1)

      (genesis :+ issue :+ sponsor, senderNotIssuer, insufficientFee, insufficientReducedFee)
    }

    val (preconditions, senderNotIssuer, _, insufficientReducedFee) = setup
    val setupBlocks                                                 = Seq(block(preconditions), block(Seq()))
    assertDiffEi(setupBlocks, block(Seq(senderNotIssuer)), s) { blockDiffEi =>
      blockDiffEi should produce("Asset was issued by other address")
    }
    assertDiffEi(setupBlocks, block(Seq(insufficientReducedFee)), s) { blockDiffEi =>
      blockDiffEi should produce("(99999 in WAVES) does not exceed minimal value of 100000 WAVES")
    }
  }

  property("cannot change sponsorship fee") {
    val s = settings(0)
    val setup = {
      val master     = TxHelpers.signer(1)
      val notSponsor = TxHelpers.signer(2)

      val sponsorTxFee = (0.001 * Constants.UnitsInWave).toLong
      val sponsorFee   = 400000L

      val genesis         = Seq(master, notSponsor).map(acc => TxHelpers.genesis(acc.toAddress, 400000000))
      val issue           = TxHelpers.issue(master, version = TxVersion.V1)
      val sponsor         = TxHelpers.sponsor(issue.asset, Some(1), master, fee = sponsorTxFee)
      val senderNotIssuer = TxHelpers.sponsor(issue.asset, Some(sponsorFee), notSponsor, fee = 1 * Constants.UnitsInWave)
      val insufficientFee = TxHelpers.sponsor(issue.asset, Some(sponsorFee), master, fee = (0.001 * Constants.UnitsInWave).toLong - 1)

      (genesis :+ issue :+ sponsor, senderNotIssuer, insufficientFee)
    }

    val (preconditions, senderNotIssuer, insufficientFee) = setup
    val setupBlocks                                       = Seq(block(preconditions))
    assertDiffEi(setupBlocks, block(Seq(senderNotIssuer)), s) { blockDiffEi =>
      blockDiffEi should produce("Asset was issued by other address")
    }
    assertDiffEi(setupBlocks, block(Seq(insufficientFee)), s) { blockDiffEi =>
      blockDiffEi should produce("(99999 in WAVES) does not exceed minimal value of 100000 WAVES")
    }
  }

  property(s"sponsor has no WAVES but receives them just in time before $BlockV5 activation") {
    val s = settings(0)
    val setup = {
      val master    = TxHelpers.signer(1)
      val recipient = TxHelpers.signer(2)

      val genesis           = TxHelpers.genesis(master.toAddress, 300000000)
      val issue             = TxHelpers.issue(master, amount = 100, decimals = 2, reissuable = false, fee = 100000000, version = TxVersion.V1)
      val sponsor           = TxHelpers.sponsor(issue.asset, Some(100), master, fee = 100000000)
      val assetTransfer     = TxHelpers.transfer(master, recipient.toAddress, issue.quantity.value, issue.asset, fee = 100000, version = TxVersion.V1)
      val wavesTransfer     = TxHelpers.transfer(master, recipient.toAddress, 99800000, fee = 100000, version = TxVersion.V1)
      val backWavesTransfer = TxHelpers.transfer(recipient, master.toAddress, 100000, feeAsset = issue.asset, fee = 100, version = TxVersion.V1)

      (genesis, issue, sponsor, assetTransfer, wavesTransfer, backWavesTransfer)
    }

    val (genesis, issue, sponsor, assetTransfer, wavesTransfer, backWavesTransfer) = setup
    assertDiffAndState(
      Seq(block(Seq(genesis, issue, sponsor, assetTransfer, wavesTransfer))),
      block(Seq(backWavesTransfer)),
      s.copy(preActivatedFeatures = s.preActivatedFeatures + (BlockV5.id -> Int.MaxValue))
    ) { case (_, state) =>
      state.balance(genesis.recipient) shouldBe 0
      state.balance(genesis.recipient, IssuedAsset(issue.id())) shouldBe issue.quantity.value
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

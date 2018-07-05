package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.it.{NodeConfigs, ReportingTestName}
import com.wavesplatform.state.Sponsorship
import org.scalatest.{Assertion, CancelAfterFailure, FreeSpec, Matchers}

class SponsorshipSuite extends FreeSpec with NodesFromDocker with Matchers with ReportingTestName with CancelAfterFailure {

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.raw("waves.blockchain.custom.functionality.blocks-for-feature-activation=1"))
      .overrideBase(_.raw("waves.blockchain.custom.functionality.feature-check-blocks-period=1"))
      .withDefault(1)
      .withSpecial(3, _.nonMiner)
      .buildNonConflicting()

  val miner             = nodes.head
  val sponsor           = nodes(1)
  val alice             = nodes(2)
  val bob               = nodes(3)
  val Waves             = 100000000L
  val Token             = 100L
  val sponsorAssetTotal = 100 * Token
  val minSponsorFee     = Token
  val TinyFee           = Token / 2
  val SmallFee          = Token + Token / 2
  val LargeFee          = 10 * Token

  def assertMinAssetFee(txId: String, sponsorship: Long): Assertion = {
    val txInfo = miner.transactionInfo(txId)
    assert(txInfo.minSponsoredAssetFee.contains(sponsorship))
  }

  def assertSponsorship(assetId: String, sponsorship: Long): Assertion = {
    val assetInfo = miner.assetsDetails(assetId)
    assert(assetInfo.minSponsoredAssetFee == Some(sponsorship).filter(_ != 0))
  }

  "Fee in sponsored asset works fine" - {

    val sponsorWavesBalance = miner.accountBalances(sponsor.address)._2
    val minerWavesBalance   = miner.accountBalances(miner.address)._2

    //25000000000000
    //24999799900000
    //24999699900000
    val sponsorAssetId =
      sponsor
        .issue(sponsor.address, "SponsoredAsset", "Created by Sponsorship Suite", sponsorAssetTotal, decimals = 2, reissuable = false, fee = issueFee)
        .id
    nodes.waitForHeightAriseAndTxPresent(sponsorAssetId)

    val transferTxToAlice = sponsor.transfer(sponsor.address, alice.address, sponsorAssetTotal / 2, minFee, Some(sponsorAssetId), None).id
    nodes.waitForHeightAriseAndTxPresent(transferTxToAlice)

    "cannot set up sponsorship" - {

      "if sender not issuer" in {
        assertBadRequestAndResponse(alice.sponsorAsset(alice.address, sponsorAssetId, baseFee = Token, fee = sponsorFee),
                                    "Asset was issued by other address")
      }

      "not enough fee" in {
        assertBadRequestAndResponse(
          sponsor.sponsorAsset(sponsor.address, sponsorAssetId, baseFee = Token, fee = sponsorFee / 2),
          "Fee in WAVES for scorex.transaction.assets.SponsorFeeTransaction does not exceed minimal value of 100000000 WAVES: 50000000"
        )
      }

      "fee is 0 or negative" in {
        assertBadRequestAndResponse(sponsor.sponsorAsset(sponsor.address, sponsorAssetId, baseFee = Token, fee = -sponsorFee), "insufficient fee")
        assertBadRequestAndResponse(sponsor.sponsorAsset(sponsor.address, sponsorAssetId, baseFee = Token, fee = 0), "insufficient fee")
      }

    }

    "make asset sponsored" in {
      val sponsorId = sponsor.sponsorAsset(sponsor.address, sponsorAssetId, baseFee = Token, fee = sponsorFee).id
      nodes.waitForHeightAriseAndTxPresent(sponsorId)
      assert(!sponsorAssetId.isEmpty)
      assert(!sponsorId.isEmpty)
      assertSponsorship(sponsorAssetId, 1 * Token)
      assertMinAssetFee(sponsorId, 1 * Token)
    }

    "check balance before test accounts balances" in {
      miner.assertAssetBalance(sponsor.address, sponsorAssetId, sponsorAssetTotal / 2)
      miner.assertBalances(sponsor.address, sponsorWavesBalance - 2 * issueFee - minFee)
      miner.assertAssetBalance(alice.address, sponsorAssetId, sponsorAssetTotal / 2)

      val assetInfo = alice.assetsBalance(alice.address).balances.filter(_.assetId == sponsorAssetId).head
      assetInfo.minSponsoredAssetFee shouldBe Some(Token)
      assetInfo.sponsorBalance shouldBe Some(sponsor.accountBalances(sponsor.address)._2)
    }

    "cannot change sponsorship fee" - {

      "sender not issuer. cannot change sponsorship fee" in {
        assertBadRequestAndResponse(alice.sponsorAsset(alice.address, sponsorAssetId, baseFee = 2 * Token, fee = sponsorFee),
                                    "Asset was issued by other address")
      }

      "not enough fee to change shonsorship" in {
        val lessThanMinFee = minFee / 2
        assertBadRequestAndResponse(
          sponsor.sponsorAsset(sponsor.address, sponsorAssetId, baseFee = 2 * Token, fee = lessThanMinFee),
          s"Fee in WAVES for scorex.transaction.assets.SponsorFeeTransaction does not exceed minimal value of 100000000 WAVES: $lessThanMinFee"
        )
      }

      "fee is zero or negative" in {
        assertBadRequestAndResponse(sponsor.sponsorAsset(sponsor.address, sponsorAssetId, baseFee = Token, fee = -sponsorFee), "insufficient fee")
        assertBadRequestAndResponse(sponsor.sponsorAsset(sponsor.address, sponsorAssetId, baseFee = Token, fee = 0), "insufficient fee")
      }
    }

    "cannot cancel sponsorship" - {
      "canceller not issuer" in {
        assertBadRequestAndResponse(alice.cancelSponsorship(alice.address, sponsorAssetId, fee = sponsorFee), "Asset was issued by other address")
      }
      "not enough fee for cancel" in {
        val lessThenMinSponsorFee = sponsorFee / 2
        assertBadRequestAndResponse(
          sponsor.cancelSponsorship(sponsor.address, sponsorAssetId, fee = lessThenMinSponsorFee),
          s"Fee in WAVES for scorex.transaction.assets.SponsorFeeTransaction does not exceed minimal value of 100000000 WAVES: $lessThenMinSponsorFee"
        )
      }

      "fee is zero or negative" in {
        assertBadRequestAndResponse(sponsor.cancelSponsorship(sponsor.address, sponsorAssetId, fee = -sponsorFee), "insufficient fee")
        assertBadRequestAndResponse(sponsor.cancelSponsorship(sponsor.address, sponsorAssetId, fee = 0), "insufficient fee")
      }

    }

    "sender cannot make transfer" - {
      "transfer tx sponsored fee is less then minimal" in {
        assertBadRequestAndResponse(
          sponsor
            .transfer(sponsor.address, alice.address, 10 * Token, fee = TinyFee, assetId = Some(sponsorAssetId), feeAssetId = Some(sponsorAssetId))
            .id,
          s"Fee in $sponsorAssetId .* does not exceed minimal value"
        )
      }

      "not enought balance for fee" in {
        assertBadRequestAndResponse(bob.transfer(bob.address, alice.address, 1.waves, SmallFee, None, Some(sponsorAssetId)), "unavailable funds")
      }

      "if sponsor has not enough spendable and effective balance to pay fee" in {
        val (sponsorBalance, sponsorEffectiveBalance) = sponsor.accountBalances(sponsor.address)
        val sponsorLeaseAllAvaliableWaves             = sponsor.lease(sponsor.address, bob.address, sponsorEffectiveBalance - leasingFee, leasingFee).id
        nodes.waitForHeightAriseAndTxPresent(sponsorLeaseAllAvaliableWaves)
        assertBadRequestAndResponse(alice.transfer(alice.address, bob.address, 10 * Token, LargeFee, Some(sponsorAssetId), Some(sponsorAssetId)),
                                    "negative effective balance")
        val bobLeaseSomeWavesToSponsorTx = bob.lease(bob.address, sponsor.address, leasingAmount, leasingFee).id
        nodes.waitForHeightAriseAndTxPresent(bobLeaseSomeWavesToSponsorTx)

        assertBadRequestAndResponse(alice.transfer(alice.address, bob.address, 10 * Token, LargeFee, Some(sponsorAssetId), Some(sponsorAssetId)),
                                    "leased being more than own")
        val cancelBobLeasingTx = bob.cancelLease(bob.address, bobLeaseSomeWavesToSponsorTx, leasingFee).id

        val cancelSponsorLeasingTx = sponsor.cancelLease(sponsor.address, sponsorLeaseAllAvaliableWaves, leasingFee).id
        nodes.waitForHeightAriseAndTxPresent(cancelSponsorLeasingTx)
      }

      "sponsor sends all waves using sponsor fee" in {
        val (sponsorBalance, sponsorEffectiveBalance) = sponsor.accountBalances(sponsor.address)
        assertBadRequestAndResponse(sponsor.transfer(sponsor.address, bob.address, sponsorBalance, SmallFee, None, Some(sponsorAssetId)),
                                    "negative waves balance")
      }

      "negative sponsored fee" in {
        assertBadRequestAndResponse(alice.transfer(alice.address, bob.address, 10 * Token, -LargeFee, Some(sponsorAssetId), Some(sponsorAssetId)),
                                    "insufficient fee")
      }

      "invalid tx timestamp" in {
        //todo

      }
    }

    val minerWavesBalanceAfterFirstXferTest   = minerWavesBalance + 2.waves + minFee + 4 * leasingFee + Sponsorship.FeeUnit * SmallFee / minSponsorFee
    val sponsorWavesBalanceAfterFirstXferTest = sponsorWavesBalance - 2.waves - minFee - 2 * leasingFee - Sponsorship.FeeUnit * SmallFee / minSponsorFee

    "fee should be written off in issued asset" - {

      "alice transfer sponsored asset to bob using sponsored fee" in {
        val transferTxCustomFeeAlice = alice.transfer(alice.address, bob.address, 10 * Token, SmallFee, Some(sponsorAssetId), Some(sponsorAssetId)).id
        nodes.waitForHeightAriseAndTxPresent(transferTxCustomFeeAlice)
        assert(!transferTxCustomFeeAlice.isEmpty)
        miner.assertAssetBalance(alice.address, sponsorAssetId, sponsorAssetTotal / 2 - SmallFee - 10 * Token)
        miner.assertAssetBalance(bob.address, sponsorAssetId, 10 * Token)
      }

      "check transaction info" in {
        //todo
        //        Transaction info for Sponsorship
        //        Transaction info for Transfer ()
      }

      "check transactions by address" in {
        //todo
        //Transaction by address for Transfer tx (miner - no, sponsor - no, sender - yes)
        //        Transaction by address for Sponsorship
      }

      "sponsor should receive sponsored asset as fee, waves should be written off" in {
        miner.assertAssetBalance(sponsor.address, sponsorAssetId, sponsorAssetTotal / 2 + SmallFee)
        miner.assertBalances(sponsor.address, sponsorWavesBalanceAfterFirstXferTest)
      }

      "miner waves balance should be changed" in {
        miner.assertBalances(miner.address, minerWavesBalanceAfterFirstXferTest)
      }
    }

    "assets balance should contain sponsor fee info and sponsor balance" in {
      val sponsorLeaseSomeWaves = sponsor.lease(sponsor.address, bob.address, leasingAmount, leasingFee).id
      nodes.waitForHeightAriseAndTxPresent(sponsorLeaseSomeWaves)
      val (sponsorBalance, sponsorEffectiveBalance) = sponsor.accountBalances(sponsor.address)
      val assetsBalance                             = alice.assetsBalance(alice.address).balances.filter(_.assetId == sponsorAssetId).head
      assetsBalance.minSponsoredAssetFee shouldBe Some(minSponsorFee)
      assetsBalance.sponsorBalance shouldBe Some(sponsorEffectiveBalance)
    }

    "waves fee depends on sponsor fee and sponsored token decimals" in {
      val transferTxCustomFeeAlice = alice.transfer(alice.address, bob.address, 1.waves, LargeFee, None, Some(sponsorAssetId)).id
      nodes.waitForHeightAriseAndTxPresent(transferTxCustomFeeAlice)
      assert(!transferTxCustomFeeAlice.isEmpty)

      miner.assertAssetBalance(sponsor.address, sponsorAssetId, sponsorAssetTotal / 2 + SmallFee + LargeFee)
      miner.assertAssetBalance(alice.address, sponsorAssetId, sponsorAssetTotal / 2 - SmallFee - LargeFee - 10 * Token)
      miner.assertAssetBalance(bob.address, sponsorAssetId, 10 * Token)
      miner.assertBalances(
        sponsor.address,
        sponsorWavesBalanceAfterFirstXferTest - Sponsorship.FeeUnit * LargeFee / Token - leasingFee,
        sponsorWavesBalanceAfterFirstXferTest - Sponsorship.FeeUnit * LargeFee / Token - leasingFee - leasingAmount
      )
      miner.assertBalances(miner.address, minerWavesBalanceAfterFirstXferTest + Sponsorship.FeeUnit * LargeFee / Token + leasingFee)
    }

    "cancel sponsorship" - {

      "cancel" in {
        val cancelSponsorshipTxId = sponsor.cancelSponsorship(sponsor.address, sponsorAssetId, fee = issueFee).id
        nodes.waitForHeightAriseAndTxPresent(cancelSponsorshipTxId)
        assert(!cancelSponsorshipTxId.isEmpty)
      }

      "check asset details info" in {
        val assetInfo = alice.assetsBalance(alice.address).balances.filter(_.assetId == sponsorAssetId).head
        assetInfo.minSponsoredAssetFee shouldBe None
        assetInfo.sponsorBalance shouldBe None
      }

      "cannot pay fees in non sponsored assets" in {
        assertBadRequestAndResponse(
          alice.transfer(alice.address, bob.address, 10 * Token, fee = 1 * Token, assetId = None, feeAssetId = Some(sponsorAssetId)).id,
          s"Asset $sponsorAssetId is not sponsored, cannot be used to pay fees"
        )
      }

      "check cancel transaction info" in {
        assertSponsorship(sponsorAssetId, 0L)
      }

      "check sponsor and miner balances after cancel" in {
        miner.assertBalances(
          sponsor.address,
          sponsorWavesBalanceAfterFirstXferTest - Sponsorship.FeeUnit * LargeFee / Token - leasingFee - issueFee,
          sponsorWavesBalanceAfterFirstXferTest - Sponsorship.FeeUnit * LargeFee / Token - leasingFee - leasingAmount - issueFee
        )
        miner.assertBalances(miner.address, minerWavesBalanceAfterFirstXferTest + Sponsorship.FeeUnit * LargeFee / Token + leasingFee + issueFee)
      }

      "cancel sponsopship again" in {
        val cancelSponsorshipTxId = sponsor.cancelSponsorship(sponsor.address, sponsorAssetId, fee = issueFee).id
        nodes.waitForHeightAriseAndTxPresent(cancelSponsorshipTxId)
        assert(!cancelSponsorshipTxId.isEmpty)
      }

    }
    "set sponsopship again" - {

      "set sponorship and check new asset details, min sponsored fee changed" in {
        val setAssetSponsoredTx = sponsor.sponsorAsset(sponsor.address, sponsorAssetId, fee = issueFee, baseFee = TinyFee).id
        nodes.waitForHeightAriseAndTxPresent(setAssetSponsoredTx)
        assert(!setAssetSponsoredTx.isEmpty)
        val assetInfo = alice.assetsBalance(alice.address).balances.filter(_.assetId == sponsorAssetId).head
        assetInfo.minSponsoredAssetFee shouldBe Some(Token / 2)
        assetInfo.sponsorBalance shouldBe Some(sponsor.accountBalances(sponsor.address)._2)
      }

      "make transfer with new min sponsored fee" in {
        val sponsoredBalance    = sponsor.accountBalances(sponsor.address)
        val sponsorAssetBalance = sponsor.assetBalance(sponsor.address, sponsorAssetId).balance
        val aliceAssetBalance   = alice.assetBalance(alice.address, sponsorAssetId).balance
        val aliceWavesBalance   = alice.accountBalances(alice.address)
        val bobAssetBalance     = bob.assetBalance(bob.address, sponsorAssetId).balance
        val bobWavesBalance     = bob.accountBalances(bob.address)
        val minerBalance        = miner.accountBalances(miner.address)
        val minerAssetBalance   = miner.assetBalance(miner.address, sponsorAssetId).balance

        val transferTxCustomFeeAlice = alice.transfer(alice.address, bob.address, 1.waves, TinyFee, None, Some(sponsorAssetId)).id
        nodes.waitForHeightAriseAndTxPresent(transferTxCustomFeeAlice)
//24998498150000
//24999498050000
        val wavesFee = Sponsorship.FeeUnit * TinyFee / TinyFee
        sponsor.assertBalances(sponsor.address, sponsoredBalance._1 - wavesFee, sponsoredBalance._2 - wavesFee)
        sponsor.assertAssetBalance(sponsor.address, sponsorAssetId, sponsorAssetBalance + TinyFee)
        alice.assertAssetBalance(alice.address, sponsorAssetId, aliceAssetBalance - TinyFee)
        alice.assertBalances(alice.address, aliceWavesBalance._2 - 1.waves)
        bob.assertBalances(bob.address, bobWavesBalance._1 + 1.waves, bobWavesBalance._2 + 1.waves)
        bob.assertAssetBalance(bob.address, sponsorAssetId, bobAssetBalance)
        miner.assertBalances(miner.address, minerBalance._2 + wavesFee)
        miner.assertAssetBalance(miner.address, sponsorAssetId, minerAssetBalance)
      }

      "change sponsorship fee in active sponsored asset" in {
        val setAssetSponsoredTx = sponsor.sponsorAsset(sponsor.address, sponsorAssetId, fee = issueFee, baseFee = LargeFee).id
        nodes.waitForHeightAriseAndTxPresent(setAssetSponsoredTx)
        assert(!setAssetSponsoredTx.isEmpty)
        val assetInfo = alice.assetsBalance(alice.address).balances.filter(_.assetId == sponsorAssetId).head
        assetInfo.minSponsoredAssetFee shouldBe Some(LargeFee)
        assetInfo.sponsorBalance shouldBe Some(sponsor.accountBalances(sponsor.address)._2)
      }

      "transfer tx sponsored fee is less then new minimal" in {
        assertBadRequestAndResponse(
          sponsor
            .transfer(sponsor.address, alice.address, 10 * Token, fee = SmallFee, assetId = Some(sponsorAssetId), feeAssetId = Some(sponsorAssetId))
            .id,
          s"Fee in $sponsorAssetId .* does not exceed minimal value"
        )
      }

      "make transfer with updated min sponsored fee" in {
        val sponsorFee = 3 * Token / 2

        val sponsoredBalance    = sponsor.accountBalances(sponsor.address)
        val sponsorAssetBalance = sponsor.assetBalance(sponsor.address, sponsorAssetId).balance
        val aliceAssetBalance   = alice.assetBalance(alice.address, sponsorAssetId).balance
        val aliceWavesBalance   = alice.accountBalances(alice.address)
        val bobWavesBalance     = bob.accountBalances(bob.address)
        val minerBalance        = miner.accountBalances(miner.address)

        val transferTxCustomFeeAlice = alice.transfer(alice.address, bob.address, 1.waves, LargeFee, None, Some(sponsorAssetId)).id
        nodes.waitForHeightAriseAndTxPresent(transferTxCustomFeeAlice)
        val wavesFee = Sponsorship.FeeUnit * LargeFee / LargeFee
        sponsor.assertBalances(sponsor.address, sponsoredBalance._1 - wavesFee, sponsoredBalance._2 - wavesFee)
        sponsor.assertAssetBalance(sponsor.address, sponsorAssetId, sponsorAssetBalance + LargeFee)
        alice.assertAssetBalance(alice.address, sponsorAssetId, aliceAssetBalance - LargeFee)
        alice.assertBalances(alice.address, aliceWavesBalance._2 - 1.waves)
        bob.assertBalances(bob.address, bobWavesBalance._1 + 1.waves, bobWavesBalance._2 + 1.waves)
        miner.assertBalances(miner.address, minerBalance._2 + wavesFee)
      }

    }
    "issue asset make sponsor and burn and reissue" in { //todo reissue
      val sponsorBalance = sponsor.accountBalances(sponsor.address)
      val minerBalance   = miner.accountBalances(miner.address)

      val sponsorAssetId2 =
        sponsor
          .issue(sponsor.address, "Another", "Created by Sponsorship Suite", sponsorAssetTotal, decimals = 2, reissuable = true, fee = issueFee)
          .id
      nodes.waitForHeightAriseAndTxPresent(sponsorAssetId2)
      sponsor.sponsorAsset(sponsor.address, sponsorAssetId2, baseFee = Token, fee = sponsorFee).id
      val transferTxToAlice = sponsor.transfer(sponsor.address, alice.address, sponsorAssetTotal / 2, minFee, Some(sponsorAssetId2), None).id
      nodes.waitForHeightAriseAndTxPresent(transferTxToAlice)

      val burnTxId = sponsor.burn(sponsor.address, sponsorAssetId2, sponsorAssetTotal / 2, burnFee).id
      nodes.waitForHeightAriseAndTxPresent(burnTxId)

      val assetInfo = sponsor.assetsDetails(sponsorAssetId2)
      assetInfo.minSponsoredAssetFee shouldBe Some(Token)
      assetInfo.quantity shouldBe sponsorAssetTotal / 2

      val aliceTransferWaves = alice.transfer(alice.address, bob.address, transferAmount, SmallFee, None, Some(sponsorAssetId2)).id
      nodes.waitForHeightAriseAndTxPresent(aliceTransferWaves)

      val totalWavesFee = Sponsorship.FeeUnit * SmallFee / Token + issueFee + sponsorFee + burnFee + minFee
      miner.assertBalances(miner.address, minerBalance._1 + totalWavesFee)
      sponsor.assertBalances(sponsor.address, sponsorBalance._1 - totalWavesFee, sponsorBalance._2 - totalWavesFee)
      sponsor.assertAssetBalance(sponsor.address, sponsorAssetId2, SmallFee)
    }

    "miner is sponsor" in {
      val minerBalance = miner.accountBalances(miner.address)
      val minersSpondorAssetId =
        miner
          .issue(miner.address, "MinersAsset", "Created by Sponsorship Suite", sponsorAssetTotal, decimals = 8, reissuable = true, fee = issueFee)
          .id
      nodes.waitForHeightAriseAndTxPresent(minersSpondorAssetId)
      val makeAssetSponsoredTx = miner.sponsorAsset(miner.address, minersSpondorAssetId, baseFee = Token, fee = sponsorFee).id
      nodes.waitForHeightAriseAndTxPresent(makeAssetSponsoredTx)
      val transferTxToAlice = miner.transfer(miner.address, alice.address, sponsorAssetTotal / 2, minFee, Some(minersSpondorAssetId), None).id
      nodes.waitForHeightAriseAndTxPresent(transferTxToAlice)

      miner.assertBalances(miner.address, minerBalance._1)
      val aliceSponsoredTransferWaves = alice.transfer(alice.address, bob.address, transferAmount, SmallFee, None, Some(minersSpondorAssetId)).id
      nodes.waitForHeightAriseAndTxPresent(aliceSponsoredTransferWaves)

      miner.assertBalances(miner.address, minerBalance._1 + Sponsorship.FeeUnit * SmallFee / Token)
      miner.assertAssetBalance(miner.address, minersSpondorAssetId, sponsorAssetTotal + SmallFee)
    }

  }

}

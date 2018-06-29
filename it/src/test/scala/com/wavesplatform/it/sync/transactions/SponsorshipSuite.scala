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

  val miner   = nodes.head
  val sponsor = nodes(1)
  val alice   = nodes(2)
  val bob     = nodes(3)

  def assertMinAssetFee(txId: String, sponsorship: Long): Assertion = {
    val txInfo = miner.transactionInfo(txId)
    assert(txInfo.minSponsoredAssetFee.contains(sponsorship))
    //    val response = miner.get(s"/transactions/info/$txId")
    //    val jsv      = Json.parse(response.getResponseBody)
    //    assert((jsv \ "minSponsoredAssetFee").as[JsValue] == value)
  }

  def assertSponsorship(assetId: String, sponsorship: Long): Assertion = {
    val assetInfo = miner.assetsDetails(assetId)
    assert(assetInfo.minSponsoredAssetFee == Some(sponsorship).filter(_ != 0))
    //    val response = miner.get(s"/assets/details/$assetId")
    //    val jsv      = Json.parse(response.getResponseBody)
    //    assert((jsv \ "minSponsoredAssetFee").asOpt[Long] == Some(sponsorship).filter(_ != 0))
  }

  "Fee in sponsored asset works fine" - {
    val Waves         = 100000000L
    val Token         = 100L
    val minSponsorFee = Token
    val TinyFee       = Token / 2
    val SmallFee      = Token + Token / 2
    val minWavesFee   = 0.001.waves
    val LargeFee      = 10 * Token

    val sponsorWavesBalance = miner.accountBalances(sponsor.address)._2
    val sponsorAssetTotal   = 100 * Token
    val minerWavesBalance   = miner.accountBalances(miner.address)._2

    val sponsorAssetId =
      sponsor
        .issue(sponsor.address,
               "SponsoredAsset",
               "Created by Sponsorship Suite",
               sponsorAssetTotal,
               decimals = 2,
               reissuable = false,
               fee = 1 * Waves)
        .id
    nodes.waitForHeightAriseAndTxPresent(sponsorAssetId)

    val transferTxToAlice = sponsor.transfer(sponsor.address, alice.address, sponsorAssetTotal / 2, minWavesFee, Some(sponsorAssetId), None).id
    nodes.waitForHeightAriseAndTxPresent(transferTxToAlice)

    "cannot set up sponsorship" - {

      "if sender not issuer" in {
        assertBadRequestAndResponse(alice.sponsorAsset(alice.address, sponsorAssetId, baseFee = Token, fee = 1.waves),
                                    "Asset was issued by other address")
      }

      "not enough fee" in {
        assertBadRequestAndResponse(
          sponsor.sponsorAsset(sponsor.address, sponsorAssetId, baseFee = Token, fee = 0.5.waves),
          "Fee in WAVES for scorex.transaction.assets.SponsorFeeTransaction does not exceed minimal value of 100000000 WAVES: 50000000"
        )
      }

      "fee is 0 or negative" in {
        assertBadRequestAndResponse(sponsor.sponsorAsset(sponsor.address, sponsorAssetId, baseFee = Token, fee = -1.waves), "insufficient fee")
        assertBadRequestAndResponse(sponsor.sponsorAsset(sponsor.address, sponsorAssetId, baseFee = Token, fee = 0), "insufficient fee")
      }

    }

    "make asset sponsored" in {
      val sponsorId = sponsor.sponsorAsset(sponsor.address, sponsorAssetId, baseFee = Token, fee = 1 * Waves).id
      nodes.waitForHeightAriseAndTxPresent(sponsorId)
      assert(!sponsorAssetId.isEmpty)
      assert(!sponsorId.isEmpty)
      assertSponsorship(sponsorAssetId, 1 * Token)
      assertMinAssetFee(sponsorId, 1 * Token)
    }

    "check before test accounts balances" in {
      miner.assertAssetBalance(sponsor.address, sponsorAssetId, sponsorAssetTotal / 2)
      miner.assertBalances(sponsor.address, sponsorWavesBalance - 2.waves - minWavesFee)
      miner.assertAssetBalance(alice.address, sponsorAssetId, sponsorAssetTotal / 2)

      val assetInfo = alice.assetsBalance(alice.address).balances.filter(_.assetId == sponsorAssetId).head
      assetInfo.minSponsoredAssetFee shouldBe Some(Token)
      assetInfo.sponsorBalance shouldBe Some(sponsor.accountBalances(sponsor.address)._2)
    }

    "cannot change sponsorship fee" - {

      "sender not issuer. cannot change sponsorship fee" in {
        assertBadRequestAndResponse(alice.sponsorAsset(alice.address, sponsorAssetId, baseFee = 2 * Token, fee = 1.waves),
                                    "Asset was issued by other address")
      }

      "not enough fee to change shonsorship" in {
        assertBadRequestAndResponse(
          sponsor.sponsorAsset(sponsor.address, sponsorAssetId, baseFee = 2 * Token, fee = 0.5.waves),
          "Fee in WAVES for scorex.transaction.assets.SponsorFeeTransaction does not exceed minimal value of 100000000 WAVES: 50000000"
        )
      }

      "fee is zeoro or negative" in {
        assertBadRequestAndResponse(sponsor.sponsorAsset(sponsor.address, sponsorAssetId, baseFee = Token, fee = -1.waves), "insufficient fee")
        assertBadRequestAndResponse(sponsor.sponsorAsset(sponsor.address, sponsorAssetId, baseFee = Token, fee = 0), "insufficient fee")
      }
    }

    "cannot cancel sponsorship" - {
      "canceller not issuer" in {
        assertBadRequestAndResponse(alice.cancelSponsorship(alice.address, sponsorAssetId, fee = 1.waves), "Asset was issued by other address")
      }
      "not enouht fee for cancel" in {
        assertBadRequestAndResponse(
          sponsor.cancelSponsorship(sponsor.address, sponsorAssetId, fee = 0.5.waves),
          "Fee in WAVES for scorex.transaction.assets.SponsorFeeTransaction does not exceed minimal value of 100000000 WAVES: 50000000"
        )
      }

      "fee is zeoro or negative" in {
        assertBadRequestAndResponse(sponsor.cancelSponsorship(sponsor.address, sponsorAssetId, fee = -1.waves), "insufficient fee")
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

      "if sponsor has not enough spendable andeffective balance to pay fee" in {
        val (sponsorBalance, sponsorEffectiveBalance) = sponsor.accountBalances(sponsor.address)
        val sponsorLeaseAllAvaliableWaves             = sponsor.lease(sponsor.address, bob.address, sponsorEffectiveBalance - leasingFee, leasingFee).id
        nodes.waitForHeightAriseAndTxPresent(sponsorLeaseAllAvaliableWaves)
        assertBadRequestAndResponse(alice.transfer(alice.address, bob.address, 10 * Token, LargeFee, Some(sponsorAssetId), Some(sponsorAssetId)),
                                    "negative effective balance")

        val bobLeaseSomeWavesToSponsorTx = bob.lease(bob.address, sponsor.address, leasingAmount, leasingFee).id
        nodes.waitForHeightAriseAndTxPresent(bobLeaseSomeWavesToSponsorTx)
        assertBadRequestAndResponse(alice.transfer(alice.address, bob.address, 10 * Token, LargeFee, Some(sponsorAssetId), Some(sponsorAssetId)),
                                    "negative effective balance")

        val cancelBobLeasingTx     = bob.cancelLease(bob.address, sponsorLeaseAllAvaliableWaves, leasingFee).id
        val cancelSponsorLeasingTx = sponsor.cancelLease(sponsor.address, sponsorLeaseAllAvaliableWaves, leasingFee).id
        nodes.waitForHeightAriseAndTxPresent(cancelBobLeasingTx)
        nodes.waitForHeightAriseAndTxPresent(cancelSponsorLeasingTx)
      }

      "sponsor sends all waves using sponsor fee" in {
        val (sponsorBalance, sponsorEffectiveBalance) = sponsor.accountBalances(sponsor.address)
        assertBadRequestAndResponse(
          sponsor.transfer(sponsor.address, bob.address, sponsorBalance, SmallFee, Some(sponsorAssetId), Some(sponsorAssetId)),
          "negative effective balance")
      }

      "negative sponsored fee" in {
        assertBadRequestAndResponse(alice.transfer(alice.address, bob.address, 10 * Token, -LargeFee, Some(sponsorAssetId), Some(sponsorAssetId)),
                                    "negative effective balance")
      }

      "invalid tx timestamp" in {}
    }

    val minerWavesBalanceAfterFirstXferTest   = minerWavesBalance + 2.waves + minWavesFee + 2 * leasingFee + Sponsorship.FeeUnit * SmallFee / Token
    val sponsorWavesBalanceAfterFirstXferTest = sponsorWavesBalance - 2.waves - minWavesFee - 2 * leasingFee - Sponsorship.FeeUnit * SmallFee / Token

    "fee should be written off in issued asset" - {

      "alice transfer sponsored asset to bob using sponsored fee" in {
        val transferTxCustomFeeAlice = alice.transfer(alice.address, bob.address, 10 * Token, SmallFee, Some(sponsorAssetId), Some(sponsorAssetId)).id
        nodes.waitForHeightAriseAndTxPresent(transferTxCustomFeeAlice)
        assert(!transferTxCustomFeeAlice.isEmpty)
        miner.assertAssetBalance(alice.address, sponsorAssetId, sponsorAssetTotal / 2 - SmallFee - 10 * Token)
        miner.assertAssetBalance(bob.address, sponsorAssetId, 10 * Token)
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

    "waves fee depends on sponsor fee and total issued sponsor tokens" in {
      val transferTxCustomFeeAlice = alice.transfer(alice.address, bob.address, 1.waves, LargeFee, None, Some(sponsorAssetId)).id
      nodes.waitForHeightAriseAndTxPresent(transferTxCustomFeeAlice)
      assert(!transferTxCustomFeeAlice.isEmpty)

      miner.assertAssetBalance(sponsor.address, sponsorAssetId, sponsorAssetTotal / 2 + SmallFee + LargeFee)
      miner.assertAssetBalance(alice.address, sponsorAssetId, sponsorAssetTotal / 2 - SmallFee - LargeFee - 10 * Token)
      miner.assertAssetBalance(bob.address, sponsorAssetId, 10 * Token)
      miner.assertBalances(sponsor.address, sponsorWavesBalanceAfterFirstXferTest - Sponsorship.FeeUnit * LargeFee / Token)
      miner.assertBalances(miner.address, minerWavesBalanceAfterFirstXferTest + Sponsorship.FeeUnit * LargeFee / Token)
    }

    "cancel sponsorship" - {
      val cancelSponsorshipTxId = sponsor.cancelSponsorship(sponsor.address, sponsorAssetId, fee = 1.waves).id
      nodes.waitForHeightAriseAndTxPresent(cancelSponsorshipTxId)
      assert(!cancelSponsorshipTxId.isEmpty)

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
    }

    "issue asset make sponsor and burn" in {
      val sponsorAssetId2 =
        sponsor
          .issue(sponsor.address, "Another", "Created by Sponsorship Suite", sponsorAssetTotal, decimals = 2, reissuable = false, fee = 1 * Waves)
          .id
      nodes.waitForHeightAriseAndTxPresent(sponsorAssetId2)
      sponsor.sponsorAsset(sponsor.address, sponsorAssetId2, baseFee = Token, fee = 1 * Waves).id
      val transferTxToAlice = sponsor.transfer(sponsor.address, alice.address, sponsorAssetTotal / 2, minWavesFee, Some(sponsorAssetId2), None).id
      nodes.waitForHeightAriseAndTxPresent(transferTxToAlice)

      val burnTxId = sponsor.burn(sponsor.address, sponsorAssetId2, sponsorAssetTotal / 2, 1.waves).id
      nodes.waitForHeightAriseAndTxPresent(burnTxId)

      val assetInfo = sponsor.assetsDetails(sponsorAssetId2)
      assetInfo.minSponsoredAssetFee shouldBe Some(Token)
      assetInfo.quantity shouldBe sponsorAssetTotal / 2

      val aliceTransferWaves = alice.transfer(alice.address, bob.address, 1.waves, SmallFee, None, Some(sponsorAssetId)).id
      nodes.waitForHeightAriseAndTxPresent(aliceTransferWaves)
      miner.assertAssetBalance(sponsor.address, sponsorAssetId, SmallFee)
    }
  }
}

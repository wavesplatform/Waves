package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.wavesplatform.it.{NodeConfigs, ReportingTestName}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.{BaseTransactionSuite, NodesFromDocker}
import com.wavesplatform.state.Sponsorship
import com.wavesplatform.it.util._
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}
import play.api.libs.json.Json

class SponsorshipSuite extends FreeSpec with NodesFromDocker with Matchers with ReportingTestName with CancelAfterFailure {

  val Waves       = 100000000L
  val Token       = 100L
  val TinyFee     = Token / 2
  val SmallFee    = Token + Token / 2
  val minWavesFee = 0.001.waves
  val LargeFee    = 10 * Token

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.raw("waves.blockchain.custom.functionality.blocks-for-feature-activation=1"))
      .withDefault(1)
      .withSpecial(3, _.nonMiner)
      .buildNonConflicting()

  val miner   = nodes.head
  val sponsor = nodes(1)
  val alice   = nodes(2)
  val bob     = nodes(3)

  def assertSponsorship(assetId: String, sponsorship: Long) = {
    val response = sponsor.get(s"/assets/details/$assetId")
    val jsv      = Json.parse(response.getResponseBody)
    assert((jsv \ "sponsorship").as[Long] == sponsorship)
  }

  "Fee in sponsored asset works correctly" - {
    val sponsorWavesBalance = miner.accountBalances(sponsor.address)._2
    val sponsorAssetTotal   = 100 * Token
    val minerWavesBalance   = miner.accountBalances(miner.address)._2
    val aliceWavesBalance   = miner.accountBalances(alice.address)._2

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
    val nonSponsoredAssetId =
      sponsor
        .issue(sponsor.address, "NonSponsAsset", "Created by Sponsorship Suite", sponsorAssetTotal, decimals = 2, reissuable = false, fee = 1 * Waves)
        .id
    assert(!sponsorAssetId.isEmpty)
    assert(!nonSponsoredAssetId.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(sponsorAssetId)

    val transferTxToAlice = sponsor.transfer(sponsor.address, alice.address, sponsorAssetTotal / 2, minWavesFee, Some(sponsorAssetId), None).id
    nodes.waitForHeightAriseAndTxPresent(transferTxToAlice)

    val sponsorId = sponsor.sponsorAsset(sponsor.address, sponsorAssetId, baseFee = 1 * Token, fee = 1 * Waves).id
    assert(!sponsorId.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(sponsorId)

    assertSponsorship(sponsorAssetId, 1 * Token)

    "check before test accounts balances" in {
      miner.assertAssetBalance(sponsor.address, sponsorAssetId, sponsorAssetTotal / 2)
      miner.assertBalances(sponsor.address, sponsorWavesBalance - 3.waves - minWavesFee)
      miner.assertAssetBalance(alice.address, sponsorAssetId, sponsorAssetTotal / 2)
    }

    "not enought balance for fee" in {
      assertBadRequestAndResponse(bob.transfer(bob.address, alice.address, 1.waves, SmallFee, None, Some(sponsorAssetId)), "unavailable funds")
    }

    val minerWavesBalanceAfterFirstXferTest   = minerWavesBalance + 3.waves + minWavesFee + Sponsorship.FeeUnit * SmallFee / Token
    val sponsorWavesBalanceAfterFirstXferTest = sponsorWavesBalance - 3.waves - minWavesFee - Sponsorship.FeeUnit * SmallFee / Token

    "fee should be written off in issued asset" in {
      val transferTxCustomFeeAlice = alice.transfer(alice.address, bob.address, 10 * Token, SmallFee, Some(sponsorAssetId), Some(sponsorAssetId)).id
      nodes.waitForHeightAriseAndTxPresent(transferTxCustomFeeAlice)
      assert(!transferTxCustomFeeAlice.isEmpty)
      miner.assertAssetBalance(sponsor.address, sponsorAssetId, sponsorAssetTotal / 2 + SmallFee)
      miner.assertAssetBalance(alice.address, sponsorAssetId, sponsorAssetTotal / 2 - SmallFee - 10 * Token)
      miner.assertAssetBalance(bob.address, sponsorAssetId, 10 * Token)
      miner.assertBalances(sponsor.address, sponsorWavesBalanceAfterFirstXferTest)
      miner.assertBalances(miner.address, minerWavesBalanceAfterFirstXferTest)
    }

    "waves fee depends on sponsor fee and total sponsor tokens" in {
      val transferTxCustomFeeAlice = alice.transfer(alice.address, bob.address, 1.waves, LargeFee, None, Some(sponsorAssetId)).id
      nodes.waitForHeightAriseAndTxPresent(transferTxCustomFeeAlice)
      assert(!transferTxCustomFeeAlice.isEmpty)
      miner.assertAssetBalance(sponsor.address, sponsorAssetId, sponsorAssetTotal / 2 + SmallFee + LargeFee)
      miner.assertAssetBalance(alice.address, sponsorAssetId, sponsorAssetTotal / 2 - SmallFee - 10 * Token)
      miner.assertAssetBalance(bob.address, sponsorAssetId, 10 * Token)
      miner.assertBalances(sponsor.address, sponsorWavesBalanceAfterFirstXferTest - Sponsorship.FeeUnit * LargeFee / Token)
      miner.assertBalances(miner.address, minerWavesBalanceAfterFirstXferTest + Sponsorship.FeeUnit * LargeFee / Token)
    }

    "invalid tx if fee less then minimal" in {
      assertBadRequestAndResponse(
        sponsor
          .transfer(sponsor.address, alice.address, 10 * Token, fee = TinyFee, assetId = Some(sponsorAssetId), feeAssetId = Some(sponsorAssetId))
          .id,
        s"Fee in $sponsorAssetId .* does not exceed minimal value"
      )
    }

    "cancel sponsorship, cannot pay fees in non sponsored assets " in {
      val cancelSponsorshipTxId = sponsor.cancelSponsorship(sponsor.address, sponsorAssetId, fee = 1.waves).id
      nodes.waitForHeightAriseAndTxPresent(cancelSponsorshipTxId)
      assert(!cancelSponsorshipTxId.isEmpty)
      assertSponsorship(sponsorAssetId, 0L)
      assertBadRequestAndResponse(
        alice.transfer(alice.address, bob.address, 10 * Token, fee = 1 * Token, assetId = None, feeAssetId = Some(sponsorAssetId)).id,
        s"Asset $nonSponsoredAssetId is not sponsored, cannot be used to pay fees"
      )
    }
    //    "others" in {
    //
    //      val xfer1Id =
    //        miner.transfer(sponsor.address, alice.address, LargeFee * 2, fee = SmallFee, assetId = Some(sponsorAssetId), feeAssetId = Some(sponsorAssetId)).id
    //      assert(!xfer1Id.isEmpty)
    //      nodes.waitForHeightAriseAndTxPresent(xfer1Id)
    //      val waves1 = sponsorWavesBalance - 2 * Waves - Sponsorship.FeeUnit * SmallFee / Token
    //      miner.assertBalances(sponsor.address, waves1, waves1)
    //      val sponsorAssetBalance = sponsorAssetTotal - LargeFee * 2
    //      miner.assertAssetBalance(sponsor.address, sponsorAssetId, sponsorAssetBalance)
    //    }
    ////
    //      val xfer2Id = sender.transfer(secondAddress, thirdAddress, 10 * Token, fee = LargeFee, assetId = None, feeAssetId = Some(assetId)).id
    //      assert(!xfer2Id.isEmpty)
    //      nodes.waitForHeightAriseAndTxPresent(xfer2Id)
    //      val waves2 = waves1 - Sponsorship.FeeUnit * LargeFee / Token
    //      miner.assertBalances(firstAddress, waves2, waves2)
    //      val asset2 = sponsorAssetBalance + LargeFee
    //      miner.assertAssetBalance(firstAddress, assetId, asset2)
    //
    //      val cancelId = sender.cancelSponsorship(firstAddress, assetId, fee = 1 * Waves).id
    //      assert(!cancelId.isEmpty)
    //      nodes.waitForHeightAriseAndTxPresent(cancelId)
    //
    //      assertSponsorship(assetId, 0L)
    //
    //      assertBadRequestAndResponse(
    //        sender.transfer(secondAddress, thirdAddress, 10 * Token, fee = 1 * Token, assetId = None, feeAssetId = Some(assetId)).id,
    //        s"Asset $assetId is not sponsored, cannot be used to pay fees"
    //      )
    //
    //      // by this time, the miner should have fully collected fees for asset issue, sponsorship and both transfers
    //      val minerWaves = miner.accountBalances(miner.address)._1
    //      assert(minerWaves - minerWavesBalance >= waves2 - sponsorWavesBalance)

  }
}

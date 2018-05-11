package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.it.{NodeConfigs, ReportingTestName}
import com.wavesplatform.state.Sponsorship
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers}
import play.api.libs.json.{JsNumber, JsValue, Json}

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
      .overrideBase(_.raw("waves.blockchain.custom.functionality.feature-check-blocks-period=1"))
      .withDefault(1)
      .withSpecial(3, _.nonMiner)
      .buildNonConflicting()

  val miner   = nodes.head
  val sponsor = nodes(1)
  val alice   = nodes(2)
  val bob     = nodes(3)

  def assertMinAssetFee(txId: String, value: JsValue) = {
    val response = miner.get(s"/transactions/info/$txId")
    val jsv      = Json.parse(response.getResponseBody)
    assert((jsv \ "minSponsoredAssetFee").as[JsValue] == value)
  }

  def assertSponsorship(assetId: String, sponsorship: Long) = {
    val response = miner.get(s"/assets/details/$assetId")
    val jsv      = Json.parse(response.getResponseBody)
    assert((jsv \ "minSponsoredAssetFee").asOpt[Long] == Some(sponsorship).filter(_ != 0))
  }

  "Fee in sponsored asset works correctly" - {

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

    val sponsorId = sponsor.sponsorAsset(sponsor.address, sponsorAssetId, baseFee = Token, fee = 1 * Waves).id
    nodes.waitForHeightAriseAndTxPresent(sponsorId)

    "check before test accounts balances" in {
      assert(!sponsorAssetId.isEmpty)
      assert(!sponsorId.isEmpty)
      assertSponsorship(sponsorAssetId, 1 * Token)
      assertMinAssetFee(sponsorId, JsNumber(1 * Token))
      miner.assertAssetBalance(sponsor.address, sponsorAssetId, sponsorAssetTotal / 2)
      miner.assertBalances(sponsor.address, sponsorWavesBalance - 2.waves - minWavesFee)
      miner.assertAssetBalance(alice.address, sponsorAssetId, sponsorAssetTotal / 2)

    }

    "invalid tx if fee less then minimal" in {
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

    val minerWavesBalanceAfterFirstXferTest   = minerWavesBalance + 2.waves + minWavesFee + Sponsorship.FeeUnit * SmallFee / Token
    val sponsorWavesBalanceAfterFirstXferTest = sponsorWavesBalance - 2.waves - minWavesFee - Sponsorship.FeeUnit * SmallFee / Token

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
      miner.assertAssetBalance(alice.address, sponsorAssetId, sponsorAssetTotal / 2 - SmallFee - LargeFee - 10 * Token)
      miner.assertAssetBalance(bob.address, sponsorAssetId, 10 * Token)
      miner.assertBalances(sponsor.address, sponsorWavesBalanceAfterFirstXferTest - Sponsorship.FeeUnit * LargeFee / Token)
      miner.assertBalances(miner.address, minerWavesBalanceAfterFirstXferTest + Sponsorship.FeeUnit * LargeFee / Token)
    }

    "cancel sponsorship, cannot pay fees in non sponsored assets " in {
      val cancelSponsorshipTxId = sponsor.cancelSponsorship(sponsor.address, sponsorAssetId, fee = 1.waves).id
      nodes.waitForHeightAriseAndTxPresent(cancelSponsorshipTxId)
      assert(!cancelSponsorshipTxId.isEmpty)
      assertSponsorship(sponsorAssetId, 0L)
      assertBadRequestAndResponse(
        alice.transfer(alice.address, bob.address, 10 * Token, fee = 1 * Token, assetId = None, feeAssetId = Some(sponsorAssetId)).id,
        s"Asset $sponsorAssetId is not sponsored, cannot be used to pay fees"
      )
    }

  }
}

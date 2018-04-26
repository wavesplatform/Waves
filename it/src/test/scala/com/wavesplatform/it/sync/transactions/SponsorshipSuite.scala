package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.state.Sponsorship
import play.api.libs.json.{JsNull, JsNumber, JsValue, Json}

class SponsorshipSuite extends BaseTransactionSuite {

  val Waves    = 100000000L
  val Token    = 100L
  val TinyFee  = Token / 2
  val SmallFee = Token + Token / 2
  val LargeFee = 10 * Token

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.raw("waves.blockchain.custom.functionality.blocks-for-feature-activation=1"))
      .overrideBase(_.raw("waves.blockchain.custom.functionality.feature-check-blocks-period=1"))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

  val miner = nodes.head

  private def assertMinAssetFee(txId: String, value: JsValue) = {
    val response = sender.get(s"/transactions/info/$txId")
    val jsv      = Json.parse(response.getResponseBody)
    assert((jsv \ "minAssetFee").as[JsValue] == value)
  }

  private def assertSponsorship(assetId: String, sponsorship: Long) = {
    val response = sender.get(s"/assets/details/$assetId")
    val jsv      = Json.parse(response.getResponseBody)
    assert((jsv \ "minAssetFee").asOpt[Long] == Some(sponsorship).filter(_ != 0))
  }

  test("Fee in sponsored asset works correctly") {
    val waves0      = miner.accountBalances(firstAddress)._1
    val asset0      = 100 * Token
    val minerWaves0 = miner.accountBalances(miner.address)._1

    val assetId =
      sender.issue(firstAddress, "SponsoredAsset", "Created by Sponsorship Suite", asset0, decimals = 2, reissuable = false, fee = 1 * Waves).id
    assert(!assetId.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(assetId)

    val sponsorId = sender.sponsorAsset(firstAddress, assetId, baseFee = 1 * Token, fee = 1 * Waves).id
    assert(!sponsorId.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(sponsorId)

    assertMinAssetFee(sponsorId, JsNumber(1 * Token))
    assertSponsorship(assetId, 1 * Token)

    assertBadRequestAndResponse(
      sender.transfer(firstAddress, secondAddress, 10 * Token, fee = TinyFee, assetId = Some(assetId), feeAssetId = Some(assetId)).id,
      s"Fee in $assetId .* does not exceed minimal value"
    )

    val xfer1Id = sender.transfer(firstAddress, secondAddress, LargeFee * 2, fee = SmallFee, assetId = Some(assetId), feeAssetId = Some(assetId)).id
    assert(!xfer1Id.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(xfer1Id)
    val waves1 = waves0 - 2 * Waves - Sponsorship.FeeUnit * SmallFee / Token
    miner.assertBalances(firstAddress, waves1, waves1)
    val asset1 = asset0 - LargeFee * 2
    miner.assertAssetBalance(firstAddress, assetId, asset1)

    val xfer2Id = sender.transfer(secondAddress, thirdAddress, 10 * Token, fee = LargeFee, assetId = None, feeAssetId = Some(assetId)).id
    assert(!xfer2Id.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(xfer2Id)
    val waves2 = waves1 - Sponsorship.FeeUnit * LargeFee / Token
    miner.assertBalances(firstAddress, waves2, waves2)
    val asset2 = asset1 + LargeFee
    miner.assertAssetBalance(firstAddress, assetId, asset2)

    val cancelId = sender.cancelSponsorship(firstAddress, assetId, fee = 1 * Waves).id
    assert(!cancelId.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(cancelId)

    assertMinAssetFee(cancelId, JsNull)
    assertSponsorship(assetId, 0L)

    assertBadRequestAndResponse(
      sender.transfer(secondAddress, thirdAddress, 10 * Token, fee = 1 * Token, assetId = None, feeAssetId = Some(assetId)).id,
      s"Asset $assetId is not sponsored, cannot be used to pay fees"
    )

    // by this time, the miner should have fully collected fees for asset issue, sponsorship and both transfers
    val minerWaves = miner.accountBalances(miner.address)._1
    assert(minerWaves - minerWaves0 >= waves2 - waves0)
  }
}

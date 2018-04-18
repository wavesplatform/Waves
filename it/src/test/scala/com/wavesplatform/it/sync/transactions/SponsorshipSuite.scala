package com.wavesplatform.it.sync.transactions

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.state2.Sponsorship

class SponsorshipSuite extends BaseTransactionSuite {

  val Waves    = 100000000L
  val Token    = 100L
  val TinyFee  = Token / 2
  val SmallFee = Token + Token / 2
  val LargeFee = 10 * Token

  test("Fee in sponsored asset works correctly") {
    val waves0 = notMiner.accountBalances(firstAddress)._1
    val asset0 = 100 * Token

    val ns    = 0 to 3 map { nodes(_).address }
    val miner = nodes.head

    Console.err.println(s"0: " + ns.map(miner.accountBalances(_)._1)) ///

    val assetId =
      sender.issue(firstAddress, "SponsoredAsset", "Created by Sponsorship Suite", asset0, decimals = 2, reissuable = false, fee = 1 * Waves).id
    assert(!assetId.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(assetId)

    val sponsorId = sender.sponsorAsset(firstAddress, assetId, baseFee = 1 * Token, fee = 1 * Waves).id
    assert(!sponsorId.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(sponsorId)

    assertBadRequestAndResponse(
      sender.transfer(firstAddress, secondAddress, 10 * Token, fee = TinyFee, assetId = Some(assetId), feeAssetId = Some(assetId)).id,
      s"Fee in $assetId .* does not exceed minimal value"
    )

    val xfer1Id = sender.transfer(firstAddress, secondAddress, LargeFee * 2, fee = SmallFee, assetId = Some(assetId), feeAssetId = Some(assetId)).id
    assert(!xfer1Id.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(xfer1Id)
    val waves1 = waves0 - 2 * Waves - Sponsorship.FeeUnit * SmallFee / Token
    notMiner.assertBalances(firstAddress, waves1, waves1)
    assert(notMiner.accountBalances(firstAddress)._1 == waves1) ///
    val asset1 = asset0 - LargeFee * 2
    notMiner.assertAssetBalance(firstAddress, assetId, asset1)
    assert(notMiner.assetBalance(firstAddress, assetId).balance == asset1) ///

    Console.err.println(s"1: " + ns.map(miner.accountBalances(_)._1)) ///

    val xfer2Id = sender.transfer(secondAddress, thirdAddress, 10 * Token, fee = LargeFee, assetId = None, feeAssetId = Some(assetId)).id
    assert(!xfer2Id.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(xfer2Id)
    val waves2 = waves1 - Sponsorship.FeeUnit * LargeFee / Token
    assert(notMiner.accountBalances(firstAddress)._1 == waves2) ///
    val asset2 = asset1 + LargeFee
    assert(notMiner.assetBalance(firstAddress, assetId).balance == asset2) ///

    Console.err.println(s"2: " + ns.map(miner.accountBalances(_)._1)) ///

    val cancelId = sender.cancelSponsorship(firstAddress, assetId, fee = 1 * Waves).id
    assert(!cancelId.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(cancelId)

    assertBadRequestAndResponse(
      sender.transfer(secondAddress, thirdAddress, 10 * Token, fee = 1 * Token, assetId = None, feeAssetId = Some(assetId)).id,
      s"Asset $assetId is not sponsored, cannot be used to pay fees"
    )
  }
  /// check miner's balance
  /// asset/details API returns sponsorship info
}

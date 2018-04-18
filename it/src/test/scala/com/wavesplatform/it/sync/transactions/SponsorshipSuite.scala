package com.wavesplatform.it.sync.transactions

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite

class SponsorshipSuite extends BaseTransactionSuite {

  val Token = 100000000L

  test("///") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)

    val assetId =
      sender.issue(firstAddress, "SponsoredAsset", "Created by Sponsorship Suite", 100 * Token, decimals = 2, reissuable = false, fee = 1 * Token).id
    assert(!assetId.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(assetId)
    Console.err.println(s"issue: waves=${notMiner.accountBalances(firstAddress)._1}, asset=${notMiner.assetBalance(firstAddress, assetId).balance}") ///

    val sponsorId = sender.sponsorAsset(firstAddress, assetId, baseFee = 1 * Token, fee = 1 * Token).id
    assert(!sponsorId.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(sponsorId)
    Console.err.println(s"sponsor: waves=${notMiner.accountBalances(firstAddress)._1}, asset=${notMiner.assetBalance(firstAddress, assetId).balance}") ///

    val xfer1Id = sender.transfer(firstAddress, secondAddress, 10 * Token, fee = 1 * Token, assetId = Some(assetId), feeAssetId = Some(assetId)).id
    assert(!xfer1Id.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(xfer1Id)
    Console.err.println(s"xfer1: waves=${notMiner.accountBalances(firstAddress)._1}, asset=${notMiner.assetBalance(firstAddress, assetId).balance}") ///

    val xfer2Id = sender.transfer(secondAddress, thirdAddress, 10 * Token, fee = 1 * Token, assetId = None, feeAssetId = Some(assetId)).id
    assert(!xfer2Id.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(xfer2Id)
    Console.err.println(s"xfer2: waves=${notMiner.accountBalances(firstAddress)._1}, asset=${notMiner.assetBalance(firstAddress, assetId).balance}") ///

    val cancelId = sender.cancelSponsorship(firstAddress, assetId, fee = 1 * Token).id
    assert(!cancelId.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(cancelId)
    Console.err.println(s"cancel: waves=${notMiner.accountBalances(firstAddress)._1}, asset=${notMiner.assetBalance(firstAddress, assetId).balance}") ///

    val xfer3Id = sender.transfer(secondAddress, thirdAddress, 10 * Token, fee = 1 * Token, assetId = None, feeAssetId = Some(assetId)).id
    assert(!xfer3Id.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(xfer3Id)
    Console.err.println(s"xfer3: waves=${notMiner.accountBalances(firstAddress)._1}, asset=${notMiner.assetBalance(firstAddress, assetId).balance}") ///
  }
}

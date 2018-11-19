package com.wavesplatform.it.sync.matcher.smartcontracts

import com.typesafe.config.Config
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.matcher.market.MatcherActor
import com.wavesplatform.state.ByteStr.decodeBase58
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class ExtraFeeTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = Configs

  val trueScript = Some(ScriptCompiler("true").explicitGet()._1.bytes().base64)
  val amount     = 10L
  val price      = 100000000L

  "Execute orders with one smart account and two smart assets" - {
    // set smart account
    setContract(Some("true"), aliceAcc)

    // issue two smart assets
    val asset1 = aliceNode
      .issue(aliceAcc.address, "SmartAsset1", "Test", defaultAssetQuantity, 0, reissuable = false, issueFee, 2, trueScript)
      .id
    val asset2 = bobNode
      .issue(bobAcc.address, "SmartAsset2", "Test", defaultAssetQuantity, 0, reissuable = false, issueFee, 2, trueScript)
      .id
    val pair = //TODO не помогает от "should be reverse"
      if (MatcherActor.compare(Some(asset2.getBytes), Some(asset1.getBytes)) < 0)
        AssetPair(decodeBase58(asset1).toOption, decodeBase58(asset2).toOption)
      else
        AssetPair(decodeBase58(asset2).toOption, decodeBase58(asset1).toOption)
    Seq(asset1, asset2).foreach(matcherNode.waitForTransaction(_))

    val aliceToBobTransferId =
      aliceNode.transfer(aliceAcc.address, bobAcc.address, defaultAssetQuantity / 2, 0.009.waves, Some(asset1), None, 2).id
    val bobToAliceTransferId =
      bobNode.transfer(bobAcc.address, aliceAcc.address, defaultAssetQuantity / 2, 0.005.waves, Some(asset2), None, 2).id
    Seq(aliceToBobTransferId, bobToAliceTransferId).foreach(matcherNode.waitForTransaction(_))

    val aliceInitBalance   = matcherNode.accountBalances(aliceAcc.address)._1
    val bobInitBalance     = matcherNode.accountBalances(bobAcc.address)._1
    val matcherInitBalance = matcherNode.accountBalances(matcherNode.address)._1

    val aliceFee   = tradeFee + 2 * smartFee // 2 x "smart asset"
    val bobFee     = tradeFee + 2 * smartFee // 2 x "smart asset"
    val matcherFee = aliceFee

    "smart asset should reserve extra fee" in {
      // place counter order by smart acc
      val counter = matcherNode.placeOrder(aliceAcc, pair, SELL, amount, price, aliceFee, 2).message.id
      matcherNode.waitOrderStatus(pair, counter, "Accepted")
      // assert reserved balance
      matcherNode.reservedBalance(aliceAcc)("WAVES") shouldBe aliceFee
    }

    "assert fee" in {
      // place submitted order
      val submitted = matcherNode.placeOrder(bobAcc, pair, BUY, amount, price, bobFee, 2).message.id
      matcherNode.waitOrderStatus(pair, submitted, "Filled")
      matcherNode.waitOrderInBlockchain(submitted)
      // assert fee for alice, bob and matcher
      matcherNode.accountBalances(aliceAcc.address)._1 shouldBe aliceInitBalance - aliceFee
      matcherNode.accountBalances(bobAcc.address)._1 shouldBe bobInitBalance - bobFee
      matcherNode.accountBalances(matcherNode.address)._1 shouldBe matcherInitBalance + (aliceFee + bobFee - matcherFee)
    }
  }

}

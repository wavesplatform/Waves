package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._

class MatcherTickerTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = Configs

  matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
  nodes.waitForHeightArise()
  aliceNode.transfer(aliceNode.address, aliceAcc.address, defaultAssetQuantity, 100000, Some(UsdId.toString), None, 2)
  nodes.waitForHeightArise()

  "matcher ticker validation" - {
    "get tickers for unavailable asset should produce error" in {
      SyncMatcherHttpApi.assertNotFoundAndMessage(matcherNode.marketStatus(wctWavesPair), s"Invalid Asset ID: ${IssueWctTx.id()}")
    }

    "status of empty orderbook" in {
      //    TODO: add error message after fix of https://wavesplatform.atlassian.net/browse/NODE-1151
      //      SyncMatcherHttpApi.assertNotFoundAndMessage(matcherNode.marketStatus(wavesUsdPair), s"")
    }

    "error of non-existed order" in {
      //TODO: add error message after fix of https://wavesplatform.atlassian.net/browse/NODE-1151
      //      SyncMatcherHttpApi.assertNotFoundAndMessage(matcherNode.orderStatus(IssueUsdTx.id().toString, wavesUsdPair), s"")
    }

    "try to work with incorrect pair" in {
      val usdWavesPair = AssetPair(
        amountAsset = Some(UsdId),
        priceAsset = None
      )

      assert(
        matcherNode
          .matcherGet(s"/matcher/orderbook/${usdWavesPair.amountAssetStr}/${usdWavesPair.priceAssetStr}/status", statusCode = 301)
          .getHeader("Location")
          .contains(s"WAVES/${usdWavesPair.amountAssetStr}"))

      //TODO: add error message after fix of https://wavesplatform.atlassian.net/browse/NODE-1151
      //      SyncMatcherHttpApi.assertNotFoundAndMessage(matcherNode.placeOrder(aliceNode, usdWavesPair, OrderType.BUY, 200, 1.waves), "")
    }

    "issue tokens" in {
      val iTx = matcherNode.signedIssue(createSignedIssueRequest(IssueWctTx)).id
      nodes.waitForHeightAriseAndTxPresent(iTx)
      val bTx = bobNode.transfer(bobNode.address, bobAcc.address, defaultAssetQuantity, 100000, Some(WctId.toString), None, 2).id
      nodes.waitForHeightAriseAndTxPresent(bTx)
    }

    val bidPrice  = 200000000
    val bidAmount = 1.waves
    val askPrice  = 400000000
    val askAmount = bidAmount / 2

    "place bid order for first pair" in {
      matcherNode.placeOrder(aliceAcc, wctUsdPair, OrderType.BUY, bidPrice, bidAmount)
      val aliceOrder = matcherNode.placeOrder(aliceAcc, wctUsdPair, OrderType.BUY, bidPrice, bidAmount).message.id
      matcherNode.waitOrderStatus(wctUsdPair, aliceOrder, "Accepted")

      val r = matcherNode.marketStatus(wctUsdPair)
      r.lastPrice shouldBe None
      r.lastSide shouldBe None
      r.bid shouldBe Some(bidPrice)
      r.bidAmount shouldBe Some(2 * bidAmount)
      r.ask shouldBe None
      r.askAmount shouldBe None
    }

    "place ask order for second pair" in {
      matcherNode.placeOrder(bobAcc, wctWavesPair, OrderType.SELL, askPrice, askAmount)
      val bobOrder = matcherNode.placeOrder(bobAcc, wctWavesPair, OrderType.SELL, askPrice, askAmount).message.id
      matcherNode.waitOrderStatus(wctWavesPair, bobOrder, "Accepted")
      val r = matcherNode.marketStatus(wctWavesPair)
      r.lastPrice shouldBe None
      r.lastSide shouldBe None
      r.bid shouldBe None
      r.bidAmount shouldBe None
      r.ask shouldBe Some(askPrice)
      r.askAmount shouldBe Some(2 * askAmount)
    }

    "place ask order for first pair" in {
      matcherNode.placeOrder(bobAcc, wctUsdPair, OrderType.SELL, askPrice, askAmount)
      val bobOrder = matcherNode.placeOrder(bobAcc, wctUsdPair, OrderType.SELL, askPrice, askAmount).message.id
      matcherNode.waitOrderStatus(wctUsdPair, bobOrder, "Accepted")
      val r = matcherNode.marketStatus(wctUsdPair)
      r.lastPrice shouldBe None
      r.lastSide shouldBe None
      r.bid shouldBe Some(bidPrice)
      r.bidAmount shouldBe Some(2 * bidAmount)
      r.ask shouldBe Some(askPrice)
      r.askAmount shouldBe Some(2 * askAmount)
    }

    "match bid order for first pair" in {
      val bobOrder = matcherNode.placeOrder(bobAcc, wctUsdPair, OrderType.SELL, bidPrice, askAmount).message.id
      matcherNode.waitOrderStatus(wctUsdPair, bobOrder, "Filled")
      val r = matcherNode.marketStatus(wctUsdPair)
      r.lastPrice shouldBe Some(bidPrice)
      r.lastSide shouldBe Some("sell")
      r.bid shouldBe Some(bidPrice)
      r.bidAmount shouldBe Some(2 * bidAmount - askAmount)
      r.ask shouldBe Some(askPrice)
      r.askAmount shouldBe Some(2 * askAmount)

      val bobOrder1 = matcherNode.placeOrder(bobAcc, wctUsdPair, OrderType.SELL, bidPrice, 3 * askAmount).message.id
      matcherNode.waitOrderStatus(wctUsdPair, bobOrder1, "Filled")
      val s = matcherNode.marketStatus(wctUsdPair)
      s.lastPrice shouldBe Some(bidPrice)
      s.lastSide shouldBe Some("sell")
      s.bid shouldBe None
      s.bidAmount shouldBe None
      s.ask shouldBe Some(askPrice)
      s.askAmount shouldBe Some(2 * askAmount)
    }

    "match ask order for first pair" in {
      val aliceOrder = matcherNode.placeOrder(aliceAcc, wctUsdPair, OrderType.BUY, askPrice, bidAmount).message.id
      matcherNode.waitOrderStatus(wctUsdPair, aliceOrder, "Filled")
      val r = matcherNode.marketStatus(wctUsdPair)
      r.lastPrice shouldBe Some(askPrice)
      r.lastSide shouldBe Some("buy")
      r.bid shouldBe None
      r.bidAmount shouldBe None
      r.ask shouldBe None
      r.askAmount shouldBe None
    }

  }

}

package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransactionV1
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}

class MatcherTickerTestSuite extends MatcherSuiteBase {

  import MatcherTickerTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs.map(updatedMatcherConfig.withFallback)

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    val issueTx = node.broadcastRequest(IssueUsdTx.json())
    node.waitForTransaction(issueTx.id)
  }

  "matcher ticker validation" - {
    "get tickers for unavailable asset should produce error" in {
      SyncMatcherHttpApi.assertNotFoundAndMessage(node.marketStatus(wctWavesPair), s"The asset ${IssueEightDigitAssetTx.id()} not found")
    }

    "status of empty orderbook" in {
//    TODO: add error message after fix of https://wavesplatform.atlassian.net/browse/NODE-1151
//      SyncMatcherHttpApi.assertNotFoundAndMessage(node.marketStatus(wavesUsdPair), s"")
    }

    "error of non-existed order" in {
      //TODO: add error message after fix of https://wavesplatform.atlassian.net/browse/NODE-1151
//      SyncMatcherHttpApi.assertNotFoundAndMessage(node.orderStatus(IssueUsdTx.id().toString, wavesUsdPair), s"")
    }

    "try to work with incorrect pair" in {
      val usdWavesPair = AssetPair(UsdId, Waves)

      assert(
        node
          .matcherGet(s"/matcher/orderbook/${usdWavesPair.amountAssetStr}/${usdWavesPair.priceAssetStr}/status", statusCode = 301)
          .getHeader("Location")
          .contains(s"WAVES/${usdWavesPair.amountAssetStr}"))

      //TODO: add error message after fix of https://wavesplatform.atlassian.net/browse/NODE-1151
//      SyncMatcherHttpApi.assertNotFoundAndMessage(node.placeOrder(node, usdWavesPair, OrderType.BUY, 1.waves, 200), "")
    }

    "issue tokens" in {
      val tx = node.broadcastRequest(IssueEightDigitAssetTx.json())
      node.waitForTransaction(tx.id)
    }

    val bidPrice  = 200
    val bidAmount = 1.waves
    val askPrice  = 400
    val askAmount = bidAmount / 2

    "place bid order for first pair" in {
      node.placeOrder(alice, edUsdPair, OrderType.BUY, bidAmount, bidPrice, matcherFee)
      val aliceOrder = node.placeOrder(alice, edUsdPair, OrderType.BUY, bidAmount, bidPrice, matcherFee).message.id
      node.waitOrderStatus(edUsdPair, aliceOrder, "Accepted")

      val r = node.marketStatus(edUsdPair)
      r.lastPrice shouldBe None
      r.lastSide shouldBe None
      r.bid shouldBe Some(bidPrice)
      r.bidAmount shouldBe Some(2 * bidAmount)
      r.ask shouldBe None
      r.askAmount shouldBe None
    }

    "place ask order for second pair" in {
      node.placeOrder(bob, wctWavesPair, OrderType.SELL, askAmount, askPrice, matcherFee)
      val bobOrder = node.placeOrder(bob, wctWavesPair, OrderType.SELL, askAmount, askPrice, matcherFee).message.id
      node.waitOrderStatus(wctWavesPair, bobOrder, "Accepted")
      val r = node.marketStatus(wctWavesPair)
      r.lastPrice shouldBe None
      r.lastSide shouldBe None
      r.bid shouldBe None
      r.bidAmount shouldBe None
      r.ask shouldBe Some(askPrice)
      r.askAmount shouldBe Some(2 * askAmount)
    }

    "place ask order for first pair" in {
      node.placeOrder(bob, edUsdPair, OrderType.SELL, askAmount, askPrice, matcherFee)
      val bobOrder = node.placeOrder(bob, edUsdPair, OrderType.SELL, askAmount, askPrice, matcherFee).message.id
      node.waitOrderStatus(edUsdPair, bobOrder, "Accepted")
      val r = node.marketStatus(edUsdPair)
      r.lastPrice shouldBe None
      r.lastSide shouldBe None
      r.bid shouldBe Some(bidPrice)
      r.bidAmount shouldBe Some(2 * bidAmount)
      r.ask shouldBe Some(askPrice)
      r.askAmount shouldBe Some(2 * askAmount)
    }

    "match bid order for first pair" in {
      val bobOrder = node.placeOrder(bob, edUsdPair, OrderType.SELL, askAmount, bidPrice, matcherFee).message.id
      node.waitOrderStatus(edUsdPair, bobOrder, "Filled")
      val r = node.marketStatus(edUsdPair)
      r.lastPrice shouldBe Some(bidPrice)
      r.lastSide shouldBe Some("sell")
      r.bid shouldBe Some(bidPrice)
      r.bidAmount shouldBe Some(2 * bidAmount - askAmount)
      r.ask shouldBe Some(askPrice)
      r.askAmount shouldBe Some(2 * askAmount)

      val bobOrder1 = node.placeOrder(bob, edUsdPair, OrderType.SELL, 3 * askAmount, bidPrice, matcherFee).message.id
      node.waitOrderStatus(edUsdPair, bobOrder1, "Filled")
      val s = node.marketStatus(edUsdPair)
      s.lastPrice shouldBe Some(bidPrice)
      s.lastSide shouldBe Some("sell")
      s.bid shouldBe None
      s.bidAmount shouldBe None
      s.ask shouldBe Some(askPrice)
      s.askAmount shouldBe Some(2 * askAmount)
    }

    "match ask order for first pair" in {
      val aliceOrder = node.placeOrder(alice, edUsdPair, OrderType.BUY, bidAmount, askPrice, matcherFee).message.id
      node.waitOrderStatus(edUsdPair, aliceOrder, "Filled")
      val r = node.marketStatus(edUsdPair)
      r.lastPrice shouldBe Some(askPrice)
      r.lastSide shouldBe Some("buy")
      r.bid shouldBe None
      r.bidAmount shouldBe None
      r.ask shouldBe None
      r.askAmount shouldBe None
    }
  }

}

object MatcherTickerTestSuite {

  import ConfigFactory._

  val Decimals: Byte = 2

  val usdAssetName             = "USD-X"
  val eightDigitAssetAssetName = "Eight-X"
  val IssueUsdTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = alice,
      name = usdAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .explicitGet()

  val IssueEightDigitAssetTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = bob,
      name = eightDigitAssetAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = 8,
      reissuable = false,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .explicitGet()

  val UsdId: IssuedAsset             = IssuedAsset(IssueUsdTx.id())
  val EightDigitAssetId: IssuedAsset = IssuedAsset(IssueEightDigitAssetTx.id())

  val edUsdPair = AssetPair(
    amountAsset = EightDigitAssetId,
    priceAsset = UsdId
  )

  val wctWavesPair = AssetPair(
    amountAsset = EightDigitAssetId,
    priceAsset = Waves
  )

  val wavesUsdPair = AssetPair(
    amountAsset = Waves,
    priceAsset = UsdId
  )

  private val updatedMatcherConfig = parseString(s"""waves.matcher.price-assets = ["${UsdId.id.toString}", "WAVES"]""".stripMargin)
}

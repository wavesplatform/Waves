package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.CustomFeeTransactionSuite.defaultAssetQuantity
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransactionV1
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}
import org.scalatest._

import scala.util.Random

class MatcherTickerTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
//    with CancelAfterFailure
    with NodesFromDocker
    with ReportingTestName {

  import MatcherTickerTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  val issueTx = matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
  matcherNode.waitForTransaction(issueTx.id)

  "matcher ticker validation" - {
    "get tickers for unavailable asset should produce error" in {
      SyncMatcherHttpApi.assertNotFoundAndMessage(matcherNode.marketStatus(wctWavesPair), s"Invalid Asset ID: ${IssueEightDigitAssetTx.id()}")
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
        amountAsset = UsdId,
        priceAsset = Waves
      )

      assert(
        matcherNode
          .matcherGet(s"/matcher/orderbook/${usdWavesPair.amountAssetStr}/${usdWavesPair.priceAssetStr}/status", statusCode = 301)
          .getHeader("Location")
          .contains(s"WAVES/${usdWavesPair.amountAssetStr}"))

      //TODO: add error message after fix of https://wavesplatform.atlassian.net/browse/NODE-1151
//      SyncMatcherHttpApi.assertNotFoundAndMessage(matcherNode.placeOrder(aliceNode, usdWavesPair, OrderType.BUY, 1.waves, 200), "")
    }

    "issue tokens" in {
      val tx = matcherNode.signedIssue(createSignedIssueRequest(IssueEightDigitAssetTx))
      matcherNode.waitForTransaction(tx.id)
    }

    val bidPrice  = 200
    val bidAmount = 1.waves
    val askPrice  = 400
    val askAmount = bidAmount / 2

    "place bid order for first pair" in {
      matcherNode.placeOrder(aliceNode.privateKey, edUsdPair, OrderType.BUY, bidAmount, bidPrice, matcherFee)
      val aliceOrder = matcherNode.placeOrder(aliceNode.privateKey, edUsdPair, OrderType.BUY, bidAmount, bidPrice, matcherFee).message.id
      matcherNode.waitOrderStatus(edUsdPair, aliceOrder, "Accepted")

      val r = matcherNode.marketStatus(edUsdPair)
      r.lastPrice shouldBe None
      r.lastSide shouldBe None
      r.bid shouldBe Some(bidPrice)
      r.bidAmount shouldBe Some(2 * bidAmount)
      r.ask shouldBe None
      r.askAmount shouldBe None
    }

    "place ask order for second pair" in {
      matcherNode.placeOrder(bobNode.privateKey, wctWavesPair, OrderType.SELL, askAmount, askPrice, matcherFee)
      val bobOrder = matcherNode.placeOrder(bobNode.privateKey, wctWavesPair, OrderType.SELL, askAmount, askPrice, matcherFee).message.id
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
      matcherNode.placeOrder(bobNode.privateKey, edUsdPair, OrderType.SELL, askAmount, askPrice, matcherFee)
      val bobOrder = matcherNode.placeOrder(bobNode.privateKey, edUsdPair, OrderType.SELL, askAmount, askPrice, matcherFee).message.id
      matcherNode.waitOrderStatus(edUsdPair, bobOrder, "Accepted")
      val r = matcherNode.marketStatus(edUsdPair)
      r.lastPrice shouldBe None
      r.lastSide shouldBe None
      r.bid shouldBe Some(bidPrice)
      r.bidAmount shouldBe Some(2 * bidAmount)
      r.ask shouldBe Some(askPrice)
      r.askAmount shouldBe Some(2 * askAmount)
    }

    "match bid order for first pair" in {
      val bobOrder = matcherNode.placeOrder(bobNode.privateKey, edUsdPair, OrderType.SELL, askAmount, bidPrice, matcherFee).message.id
      matcherNode.waitOrderStatus(edUsdPair, bobOrder, "Filled")
      val r = matcherNode.marketStatus(edUsdPair)
      r.lastPrice shouldBe Some(bidPrice)
      r.lastSide shouldBe Some("sell")
      r.bid shouldBe Some(bidPrice)
      r.bidAmount shouldBe Some(2 * bidAmount - askAmount)
      r.ask shouldBe Some(askPrice)
      r.askAmount shouldBe Some(2 * askAmount)

      val bobOrder1 = matcherNode.placeOrder(bobNode.privateKey, edUsdPair, OrderType.SELL, 3 * askAmount, bidPrice, matcherFee).message.id
      matcherNode.waitOrderStatus(edUsdPair, bobOrder1, "Filled")
      val s = matcherNode.marketStatus(edUsdPair)
      s.lastPrice shouldBe Some(bidPrice)
      s.lastSide shouldBe Some("sell")
      s.bid shouldBe None
      s.bidAmount shouldBe None
      s.ask shouldBe Some(askPrice)
      s.askAmount shouldBe Some(2 * askAmount)
    }

    "match ask order for first pair" in {
      val aliceOrder = matcherNode.placeOrder(aliceNode.privateKey, edUsdPair, OrderType.BUY, bidAmount, askPrice, matcherFee).message.id
      matcherNode.waitOrderStatus(edUsdPair, aliceOrder, "Filled")
      val r = matcherNode.marketStatus(edUsdPair)
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
  import com.wavesplatform.it.NodeConfigs._

  private val ForbiddenAssetId = "FdbnAsset"
  val Decimals: Byte           = 2

  private val minerDisabled = parseString("waves.miner.enable = no")
  private val matcherConfig = parseString(s"""
                                             |waves.matcher {
                                             |  enable = yes
                                             |  account = 3HmFkAoQRs4Y3PE2uR6ohN7wS4VqPBGKv7k
                                             |  bind-address = "0.0.0.0"
                                             |  blacklisted-assets = ["$ForbiddenAssetId"]
                                             |  balance-watching.enable = yes
                                             |}""".stripMargin)

  private val _Configs: Seq[Config] = (Default.last +: Random.shuffle(Default.init).take(3))
    .zip(Seq(matcherConfig, minerDisabled, minerDisabled, empty()))
    .map { case (n, o) => o.withFallback(n) }

  private val aliceSeed = _Configs(1).getString("account-seed")
  private val bobSeed   = _Configs(2).getString("account-seed")
  private val alicePk   = PrivateKeyAccount.fromSeed(aliceSeed).right.get
  private val bobPk     = PrivateKeyAccount.fromSeed(bobSeed).right.get

  val usdAssetName             = "USD-X"
  val eightDigitAssetAssetName = "Eight-X"
  val IssueUsdTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = alicePk,
      name = usdAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val IssueEightDigitAssetTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = bobPk,
      name = eightDigitAssetAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = 8,
      reissuable = false,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

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

  private val updatedMatcherConfig = parseString(s"""
                                                    |waves.matcher {
                                                    |  price-assets = [ "${UsdId.id.base58}", "WAVES"]
                                                    |}
     """.stripMargin)

  private val Configs = _Configs.map(updatedMatcherConfig.withFallback(_))
}

package com.wavesplatform.it.sync.matcher

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory.parseString
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.assets.exchange.OrderType._
import org.scalatest._

class BlacklistedTradingTestSuite extends MatcherSuiteBase with GivenWhenThen {

  import BlacklistedTradingTestSuite._
  override protected def nodeConfigs: Seq[Config] = Configs.map(configWithBlacklisted().withFallback(_))

  private def matcher = dockerNodes().head
  private def alice   = dockerNodes()(1)
  private def bob     = dockerNodes()(2)

  Seq(IssueUsdTx, IssueWctTx, IssueEthTx, IssueBtcTx).map(createSignedIssueRequest).map(matcher.signedIssue).foreach { tx =>
    matcher.waitForTransaction(tx.id)
  }

  val (dec2, dec8) = (1000L, 1000000000L)

  "When blacklists are empty and some orders was placed" - {
    val usdOrder = matcher.placeOrder(alice.privateKey, wavesUsdPair, BUY, dec8, dec2, matcherFee).message.id
    val wctOrder = matcher.placeOrder(alice.privateKey, wctWavesPair, BUY, dec2, dec8, matcherFee).message.id
    val ethOrder = matcher.placeOrder(alice.privateKey, ethWavesPair, SELL, dec8, dec8, matcherFee).message.id
    val btcOrder = matcher.placeOrder(bob.privateKey, wavesBtcPair, SELL, dec8, dec8, matcherFee).message.id

    matcher.waitOrderStatus(wctWavesPair, btcOrder, "Accepted")

    "If some assets and addresses are blacklisted" in {
      docker.restartNode(
        matcher,
        configWithBlacklisted(
          assets = Array(WctId.toString),
          names = Array("ETH.*"),
          addresses = Array(bob.address)
        )
      )

      Then("orders for blacklisted assets are not available and new orders can't be placed")
      matcher.orderStatusExpectInvalidAssetId(wctOrder, wctWavesPair, WctId.toString)
      matcher.orderStatusExpectInvalidAssetId(ethOrder, ethWavesPair, EthId.toString)
      matcher.expectRejectedOrderPlacement(alice.privateKey, wctWavesPair, BUY, dec2, dec8)
      matcher.expectRejectedOrderPlacement(alice.privateKey, ethWavesPair, SELL, dec8, dec8)
      matcher.expectRejectedOrderPlacement(bob.privateKey, wavesBtcPair, SELL, dec8, dec8)

      And("orders of blacklisted address are still available")
      matcher.orderStatus(btcOrder, wavesBtcPair).status shouldBe "Accepted"

      And("orders for other assets are still available")
      matcher.orderStatus(usdOrder, wavesUsdPair).status shouldBe "Accepted"

      And("OrderBook for blacklisted assets is not available")
      matcher.orderBookExpectInvalidAssetId(wctWavesPair, WctId.toString)
      matcher.orderBookExpectInvalidAssetId(ethWavesPair, EthId.toString)
      matcher.orderBook(wavesBtcPair).asks.size shouldBe 1

      And("OrderHistory returns info about all orders")
      matcher.activeOrderHistory(alice.privateKey).size shouldBe 3
      matcher.activeOrderHistory(alice.privateKey).foreach(_.status shouldBe "Accepted")
      matcher.activeOrderHistory(bob.privateKey).size shouldBe 1
      matcher.activeOrderHistory(bob.privateKey).head.status shouldBe "Accepted"

      And("Trading markets have info about all asset pairs")
      matcher.tradingMarkets().markets.size shouldBe 4

      And("balances are still reserved")
      matcher.reservedBalance(alice.privateKey).size shouldBe 3
      matcher.reservedBalance(bob.privateKey).size shouldBe 1

      And("orders for other assets are still available")
      matcher.orderStatus(usdOrder, wavesUsdPair).status shouldBe "Accepted"
    }

    "And now if all blacklists are cleared" in {
      docker.restartNode(matcher, configWithBlacklisted())

      Then("OrderBook for blacklisted assets is available again")
      matcher.orderBook(wctWavesPair).bids.size shouldBe 1
      matcher.orderBook(ethWavesPair).asks.size shouldBe 1

      And("order statuses are available again")
      matcher.orderStatus(wctOrder, wctWavesPair).status shouldBe "Accepted"
      matcher.orderStatus(ethOrder, ethWavesPair).status shouldBe "Accepted"

      And("new orders can be placed")
      val newWctOrder = matcher.placeOrder(alice.privateKey, wctWavesPair, BUY, dec2, dec8, matcherFee).message.id
      val newEthOrder = matcher.placeOrder(alice.privateKey, ethWavesPair, SELL, dec8, dec8, matcherFee).message.id
      val newBtcOrder = matcher.placeOrder(bob.privateKey, wavesBtcPair, SELL, dec8, dec8, matcherFee).message.id
      matcher.waitOrderStatus(wctWavesPair, newBtcOrder, "Accepted")
      matcher.orderStatus(newWctOrder, wctWavesPair).status shouldBe "Accepted"
      matcher.orderStatus(newEthOrder, ethWavesPair).status shouldBe "Accepted"
    }

  }

}

object BlacklistedTradingTestSuite {

  def configWithBlacklisted(assets: Array[String] = Array(), names: Array[String] = Array(), addresses: Array[String] = Array()): Config = {
    def toStr(array: Array[String]): String = if (array.length == 0) "" else array.mkString("\"", "\", \"", "\"")
    parseString(s"""
                |waves.matcher {
                |  blacklisted-assets = [${toStr(assets)}]
                |  blacklisted-names = [${toStr(names)}]
                |  blacklisted-addresses = [${toStr(addresses)}]
                |}
    """.stripMargin)
  }

}

package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory.parseString
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.assets.exchange.OrderType._
import org.scalatest._

class BlacklistedTradingTestSuite extends MatcherSuiteBase with GivenWhenThen {

  import BlacklistedTradingTestSuite._

  override protected def nodeConfigs: Seq[Config] = Seq(configWithBlacklisted().withFallback(Configs.head))

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val xs = Seq(IssueUsdTx, IssueWctTx, IssueEthTx, IssueBtcTx).map(_.json()).map(node.broadcastRequest(_))
    val height = xs.map(tx => node.waitForTransaction(tx.id).height).max
    node.waitForHeight(height + 1)
  }

  "When blacklists are empty" in {
    val (dec2, dec8) = (1000L, 1000000000L)

    Then("Place some orders")
    val usdOrder  = node.placeOrder(alice, wavesUsdPair, BUY, dec8, dec2, matcherFee).message.id
    val wctOrder  = node.placeOrder(alice, wctWavesPair, BUY, dec2, dec8, matcherFee).message.id
    val ethOrder  = node.placeOrder(alice, ethWavesPair, SELL, dec8, dec8, matcherFee).message.id
    val btcOrder1 = node.placeOrder(bob, wavesBtcPair, SELL, dec8, dec8, matcherFee).message.id
    node.waitOrderStatus(wctWavesPair, btcOrder1, "Accepted")

    Then("We blacklist some assets and addresses and restart the node")
    docker.restartNode(
      node,
      configWithBlacklisted(
        assets = Array(WctId.toString),
        names = Array("ETH.*"),
        addresses = Array(bob.address)
      )
    )

    Then("orders for blacklisted assets are not available and new orders can't be placed")
    node.orderStatusExpectInvalidAssetId(wctOrder, wctWavesPair, WctId.toString, _.message.contains("is blacklisted"))
    node.orderStatusExpectInvalidAssetId(ethOrder, ethWavesPair, EthId.toString, _.message.contains("is blacklisted"))
    node.expectRejectedOrderPlacement(alice, wctWavesPair, BUY, dec2, dec8)
    node.expectRejectedOrderPlacement(alice, ethWavesPair, SELL, dec8, dec8)
    node.expectRejectedOrderPlacement(bob, wavesBtcPair, SELL, dec8, dec8)

    And("orders of blacklisted address are still available")
    node.orderStatus(btcOrder1, wavesBtcPair).status shouldBe "Accepted"

    And("orders for other assets are still available")
    node.orderStatus(usdOrder, wavesUsdPair).status shouldBe "Accepted"

    And("OrderBook for blacklisted assets is not available")
    node.orderBookExpectInvalidAssetId(wctWavesPair, WctId.toString, _.message.contains("is blacklisted"))
    node.orderBookExpectInvalidAssetId(ethWavesPair, EthId.toString, _.message.contains("is blacklisted"))
    node.orderBook(wavesBtcPair).asks.size shouldBe 1

    And("OrderHistory returns info about all orders")
    node.activeOrderHistory(alice).size shouldBe 3
    node.activeOrderHistory(alice).foreach(_.status shouldBe "Accepted")
    node.activeOrderHistory(bob).size shouldBe 1
    node.activeOrderHistory(bob).head.status shouldBe "Accepted"

    And("Trading markets have info about all asset pairs")
    node.tradingMarkets().markets.size shouldBe 4

    And("balances are still reserved")
    node.reservedBalance(alice).size shouldBe 3
    node.reservedBalance(bob).size shouldBe 1

    And("orders for other assets are still available")
    node.orderStatus(usdOrder, wavesUsdPair).status shouldBe "Accepted"

    And("order can be placed on allowed pair with blacklisted asset")
    val btcOrder2 = node.placeOrder(alice, wavesBtcPair, SELL, dec8, dec8, matcherFee).message.id
    node.waitOrderStatus(wavesBtcPair, btcOrder2, "Accepted")

    And("now if all blacklists are cleared")
    docker.restartNode(node, configWithBlacklisted())

    Then("OrderBook for blacklisted assets is available again")
    node.orderBook(wctWavesPair).bids.size shouldBe 1
    node.orderBook(ethWavesPair).asks.size shouldBe 1

    And("order statuses are available again")
    node.orderStatus(wctOrder, wctWavesPair).status shouldBe "Accepted"
    node.orderStatus(ethOrder, ethWavesPair).status shouldBe "Accepted"

    And("new orders can be placed")
    val newWctOrder = node.placeOrder(alice, wctWavesPair, BUY, dec2, dec8, matcherFee).message.id
    val newEthOrder = node.placeOrder(alice, ethWavesPair, SELL, dec8, dec8, matcherFee).message.id
    val btcOrder3   = node.placeOrder(bob, wavesBtcPair, SELL, dec8, dec8, matcherFee).message.id
    node.waitOrderStatus(wctWavesPair, btcOrder3, "Accepted")
    node.orderStatus(newWctOrder, wctWavesPair).status shouldBe "Accepted"
    node.orderStatus(newEthOrder, ethWavesPair).status shouldBe "Accepted"
  }

}

object BlacklistedTradingTestSuite {

  def configWithBlacklisted(assets: Array[String] = Array.empty,
                            names: Array[String] = Array.empty,
                            addresses: Array[String] = Array.empty,
                            allowedAssetPairs: Array[String] = Array.empty): Config = {
    def toStr(array: Array[String]): String = if (array.length == 0) "" else array.mkString("\"", "\", \"", "\"")
    parseString(s"""
                |waves.matcher {
                |  blacklisted-assets = [${toStr(assets)}]
                |  blacklisted-names = [${toStr(names)}]
                |  blacklisted-addresses = [${toStr(addresses)}]
                |  allowed-asset-pairs = [${toStr(allowedAssetPairs)}]
                |  white-list-only = no
                |}
    """.stripMargin)
  }

}

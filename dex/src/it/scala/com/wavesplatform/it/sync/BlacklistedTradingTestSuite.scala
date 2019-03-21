package com.wavesplatform.it.sync

import com.typesafe.config.ConfigFactory.parseString
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.assets.exchange.OrderType._
import org.scalatest._

import scala.collection.JavaConverters._

class BlacklistedTradingTestSuite extends MatcherSuiteBase with GivenWhenThen {

  import BlacklistedTradingTestSuite._

  private def Default: Seq[Config] = ConfigFactory.parseResources("nodes.conf").getConfigList("nodes").asScala

  override protected def nodeConfigs: Seq[Config] =
    Seq(configWithBlacklisted().withFallback(MatcherPriceAssetConfig.updatedMatcherConfig).withFallback(Default.head))

  val (dec2, dec8) = (1000L, 1000000000L)
  "issue assets" in {
    val xs = Seq(IssueUsdTx, IssueWctTx, IssueEthTx, IssueBtcTx).map(createSignedIssueRequest).map(node.signedIssue)
    xs.foreach(tx => node.waitForTransaction(tx.id))
  }

  "When blacklists are empty" in {
    Then("Place some orders")
    val usdOrder = node.placeOrder(alice, wavesUsdPair, BUY, dec8, dec2, matcherFee).message.id
    val wctOrder = node.placeOrder(alice, wctWavesPair, BUY, dec2, dec8, matcherFee).message.id
    val ethOrder = node.placeOrder(alice, ethWavesPair, SELL, dec8, dec8, matcherFee).message.id
    val btcOrder = node.placeOrder(bob, wavesBtcPair, SELL, dec8, dec8, matcherFee).message.id
    node.waitOrderStatus(wctWavesPair, btcOrder, "Accepted")

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
    node.orderStatusExpectInvalidAssetId(wctOrder, wctWavesPair, WctId.toString) //
    node.orderStatusExpectInvalidAssetId(ethOrder, ethWavesPair, EthId.toString)
    node.expectRejectedOrderPlacement(alice, wctWavesPair, BUY, dec2, dec8)
    node.expectRejectedOrderPlacement(alice, ethWavesPair, SELL, dec8, dec8)
    node.expectRejectedOrderPlacement(bob, wavesBtcPair, SELL, dec8, dec8)

    And("orders of blacklisted address are still available")
    node.orderStatus(btcOrder, wavesBtcPair).status shouldBe "Accepted"

    And("orders for other assets are still available")
    node.orderStatus(usdOrder, wavesUsdPair).status shouldBe "Accepted"

    And("OrderBook for blacklisted assets is not available")
    node.orderBookExpectInvalidAssetId(wctWavesPair, WctId.toString)
    node.orderBookExpectInvalidAssetId(ethWavesPair, EthId.toString)
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
    val newBtcOrder = node.placeOrder(bob, wavesBtcPair, SELL, dec8, dec8, matcherFee).message.id
    node.waitOrderStatus(wctWavesPair, newBtcOrder, "Accepted")
    node.orderStatus(newWctOrder, wctWavesPair).status shouldBe "Accepted"
    node.orderStatus(newEthOrder, ethWavesPair).status shouldBe "Accepted"
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

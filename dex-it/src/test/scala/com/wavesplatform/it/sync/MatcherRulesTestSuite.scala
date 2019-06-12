package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.LevelResponse
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
import com.wavesplatform.transaction.assets.exchange.OrderType.BUY

class MatcherRulesTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = {

    val matcherRulesStr =
      s"""
         |waves.matcher {
         |  matching-rules = {
         |    "$WctId-$UsdId": [
         |      {
         |        start-offset = 2
         |        merge-prices = yes
         |        tick-size    = 5
         |      }
         |    ]
         |  }
         |}
       """.stripMargin

    super.nodeConfigs.map(ConfigFactory.parseString(matcherRulesStr).withFallback)
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Seq(IssueUsdTx, IssueWctTx).map(_.json()).map(node.broadcastRequest(_)).foreach { tx =>
      node.waitForTransaction(tx.id)
    }
  }

  val (amount, price) = (1000L, PriceConstant)

  "Orders should be cancelled correctly when matcher rules are changed" in {

    // here tick size is disabled (offset = 0)
    val buyOrder1 = node.placeOrder(alice, wctUsdPair, BUY, amount, 7 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, buyOrder1, "Accepted")

    // here tick size is disabled (offset = 1)
    val buyOrder2 = node.placeOrder(alice, wctUsdPair, BUY, amount, 7 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, buyOrder2, "Accepted")

    // here tick size = 5 (offset = 2), hence new order is placed into corrected price level 5, not 7
    val buyOrder3 = node.placeOrder(alice, wctUsdPair, BUY, amount, 7 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, buyOrder3, "Accepted")

    // now there are 2 price levels
    node.orderBook(wctUsdPair).bids.map(_.price) shouldBe Seq(7 * price, 5 * price)

    // price level 5 will be deleted after cancelling of buyOrder3
    node.cancelOrder(alice, wctUsdPair, buyOrder3)
    node.waitOrderStatus(wctUsdPair, buyOrder3, "Cancelled")

    node.orderBook(wctUsdPair).bids shouldBe Seq(LevelResponse(2 * amount, 7 * price))
  }
}

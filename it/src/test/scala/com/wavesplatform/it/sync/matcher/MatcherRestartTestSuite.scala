package com.wavesplatform.it.sync.matcher

import com.typesafe.config.Config
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.matcher.config.MatcherDefaultConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.duration._
import scala.util.Random

class MatcherRestartTestSuite extends MatcherSuiteBase {
  override protected def nodeConfigs: Seq[Config] = Configs
  private def orderVersion                        = (Random.nextInt(2) + 1).toByte

  "check order execution" - {
    // Alice issues new asset
    val aliceAsset =
      aliceNode.issue(aliceAcc.address, "DisconnectCoin", "Alice's coin for disconnect tests", someAssetAmount, 0, reissuable = false, issueFee, 2).id
    matcherNode.waitForTransaction(aliceAsset)

    val aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)
    // check assets's balances
    matcherNode.assertAssetBalance(aliceAcc.address, aliceAsset, someAssetAmount)
    matcherNode.assertAssetBalance(matcherAcc.address, aliceAsset, 0)

    "make order and after matcher's restart try to cancel it" in {
      // Alice places sell order
      val aliceOrder = matcherNode
        .placeOrder(aliceAcc, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, matcherFee, orderVersion)
      aliceOrder.status shouldBe "OrderAccepted"
      val firstOrder = aliceOrder.message.id

      matcherNode.waitOrderStatus(aliceWavesPair, firstOrder, "Accepted")

      // check that order is correct
      val orders = matcherNode.orderBook(aliceWavesPair)
      orders.asks.head.amount shouldBe 500
      orders.asks.head.price shouldBe 2.waves * Order.PriceConstant

      // sell order should be in the aliceNode orderbook
      matcherNode.fullOrderHistory(aliceAcc).head.status shouldBe "Accepted"

      // reboot matcher's node
      docker.killAndStartContainer(dockerNodes().head)
      Thread.sleep(60.seconds.toMillis)

      val height = nodes.map(_.height).max

      matcherNode.waitForHeight(height + 1, 40.seconds)
      matcherNode.waitOrderStatus(aliceWavesPair, firstOrder, "Accepted")
      matcherNode.fullOrderHistory(aliceAcc).head.status shouldBe "Accepted"

      val orders1 = matcherNode.orderBook(aliceWavesPair)
      orders1.asks.head.amount shouldBe 500
      orders1.asks.head.price shouldBe 2.waves * Order.PriceConstant

      val aliceSecondOrder =
        matcherNode.placeOrder(aliceAcc, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, matcherFee, orderVersion, 5.minutes)
      aliceSecondOrder.status shouldBe "OrderAccepted"

      val orders2 = matcherNode.orderBook(aliceWavesPair)
      orders2.asks.head.amount shouldBe 1000
      orders2.asks.head.price shouldBe 2.waves * Order.PriceConstant

      val cancel = matcherNode.cancelOrder(aliceAcc, aliceWavesPair, firstOrder)
      cancel.status should be("OrderCanceled")

      val orders3 = matcherNode.orderBook(aliceWavesPair)
      orders3.asks.head.amount shouldBe 500

      matcherNode.waitOrderStatus(aliceWavesPair, firstOrder, "Cancelled")
      matcherNode.fullOrderHistory(aliceAcc).head.status shouldBe "Accepted"
    }
  }
}

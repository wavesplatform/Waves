package com.wavesplatform.it.sync

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.OrderBookResponse
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.duration._
import scala.util.Random

class MatcherRestartTestSuite extends MatcherSuiteBase {
  private def orderVersion = (Random.nextInt(2) + 1).toByte

  "check order execution" - {
    "make order and after matcher's restart try to cancel it" in {
      // Alice issues new asset
      val aliceAsset =
        node
          .broadcastIssue(alice,
                          "DisconnectCoin",
                          "Alice's coin for disconnect tests",
                          someAssetAmount,
                          0,
                          reissuable = false,
                          smartIssueFee,
                          None,
                          waitForTx = true)
          .id
      node.waitForHeight(node.height + 1)

      val aliceWavesPair = AssetPair(IssuedAsset(ByteStr.decodeBase58(aliceAsset).get), Waves)
      // check assets's balances
      node.assertAssetBalance(alice.address, aliceAsset, someAssetAmount)
      node.assertAssetBalance(matcher.address, aliceAsset, 0)

      // Alice places sell order
      val aliceOrder = node
        .placeOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, matcherFee, orderVersion)
      aliceOrder.status shouldBe "OrderAccepted"
      val firstOrder = aliceOrder.message.id

      node.waitOrderStatus(aliceWavesPair, firstOrder, "Accepted")

      // check that order is correct
      val orders = node.orderBook(aliceWavesPair)
      orders.asks.head.amount shouldBe 500
      orders.asks.head.price shouldBe 2.waves * Order.PriceConstant

      // sell order should be in the node orderbook
      node.fullOrderHistory(alice).head.status shouldBe "Accepted"

      // reboot matcher's node
      docker.killAndStartContainer(node)

      node.waitOrderStatus(aliceWavesPair, firstOrder, "Accepted")
      node.fullOrderHistory(alice).head.status shouldBe "Accepted"

      val orders1 = node.orderBook(aliceWavesPair)
      orders1.asks.head.amount shouldBe 500
      orders1.asks.head.price shouldBe 2.waves * Order.PriceConstant

      val aliceSecondOrder =
        node.placeOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, matcherFee, orderVersion, 5.minutes)
      aliceSecondOrder.status shouldBe "OrderAccepted"

      val orders2 =
        node.waitFor[OrderBookResponse]("Top ask has 1000 amount")(_.orderBook(aliceWavesPair), _.asks.head.amount == 1000, 1.second)
      orders2.asks.head.price shouldBe 2.waves * Order.PriceConstant

      val cancel = node.cancelOrder(alice, aliceWavesPair, firstOrder)
      cancel.status should be("OrderCanceled")

      val orders3 = node.orderBook(aliceWavesPair)
      orders3.asks.head.amount shouldBe 500

      node.waitOrderStatus(aliceWavesPair, firstOrder, "Cancelled")
      node.fullOrderHistory(alice).head.status shouldBe "Accepted"
    }
  }
}

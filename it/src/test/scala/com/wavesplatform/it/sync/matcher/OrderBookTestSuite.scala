package com.wavesplatform.it.sync.matcher

import com.typesafe.config.Config
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.matcher.config.MatcherDefaultConfig._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.assets.exchange.Order._
import com.wavesplatform.transaction.assets.exchange.OrderType._
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

class OrderBookTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with NodesFromDocker with ReportingTestName {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head
  private def aliceNode   = nodes(1)
  private def bobNode     = nodes(2)

  "Test OrderBook" - {
    // Alice issues new assets
    val aliceAsset = aliceNode
      .issue(aliceNode.address, "AliceCoin", "AliceCoin for matcher's tests", 1000, 0, reissuable = false, 100000000L)
      .id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)

    val aliceAsset2 = aliceNode
      .issue(aliceNode.address, "AliceCoin2", "AliceCoin2 for matcher's tests", 1000, 0, reissuable = false, 100000000L)
      .id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset2)

    val aliceWavesPair  = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)
    val aliceWavesPair2 = AssetPair(ByteStr.decodeBase58(aliceAsset2).toOption, None)

    "when delete an order book then orders should be canceled and reserved balances should be released" in {
      val sellOrder        = matcherNode.placeOrder(aliceNode, aliceWavesPair, SELL, 10, 2.waves * PriceConstant).message.id
      val anotherSellOrder = matcherNode.placeOrder(aliceNode, aliceWavesPair, SELL, 10, 2.waves * PriceConstant).message.id

      val buyOrder = matcherNode.placeOrder(bobNode, aliceWavesPair, BUY, 15, 1.waves * PriceConstant).message.id

      val orderForAnotherPair = matcherNode.placeOrder(aliceNode, aliceWavesPair2, SELL, 777, 2.waves * PriceConstant).message.id

      val submitted = matcherNode.placeOrder(bobNode, aliceWavesPair, BUY, 5, 2.waves * PriceConstant).message.id
      matcherNode.waitOrderStatus(aliceWavesPair, submitted, "Filled")

      matcherNode.orderStatus(sellOrder, aliceWavesPair).status shouldBe "PartiallyFilled"
      matcherNode.orderStatus(anotherSellOrder, aliceWavesPair).status shouldBe "Accepted"
      matcherNode.orderStatus(buyOrder, aliceWavesPair).status shouldBe "Accepted"
      matcherNode.orderStatus(orderForAnotherPair, aliceWavesPair).status shouldBe "Accepted"

      matcherNode.reservedBalance(aliceNode)(aliceAsset) should be > 0L
      matcherNode.reservedBalance(bobNode)("WAVES") should be > 0L
      matcherNode.reservedBalance(aliceNode)(aliceAsset2) should be > 0L

      matcherNode.deleteOrderBook(aliceWavesPair)

      matcherNode.waitOrderStatus(aliceWavesPair, sellOrder, "Cancelled")
      matcherNode.waitOrderStatus(aliceWavesPair, buyOrder, "Cancelled")
      matcherNode.orderStatus(orderForAnotherPair, aliceWavesPair2).status shouldBe "Accepted"

      matcherNode.orderBook(aliceWavesPair).asks shouldBe empty
      matcherNode.orderBook(aliceWavesPair).bids shouldBe empty
      matcherNode.orderBook(aliceWavesPair2).asks shouldNot be(empty)

      matcherNode.reservedBalance(aliceNode) shouldBe Map(aliceAsset2 -> 777)
      matcherNode.reservedBalance(bobNode) shouldBe empty
    }

  }

}

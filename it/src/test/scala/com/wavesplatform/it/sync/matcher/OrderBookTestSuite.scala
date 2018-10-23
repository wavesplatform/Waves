package com.wavesplatform.it.sync.matcher

import com.typesafe.config.Config
import com.wavesplatform.it.{Node, ReportingTestName}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.transaction.assets.exchange.OrderType._
import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers}

class OrderBookTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with NodesFromDocker with ReportingTestName {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head
  private def aliceNode   = nodes(1)
  private def bobNode     = nodes(2)

  matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
  matcherNode.signedIssue(createSignedIssueRequest(IssueWctTx))
  nodes.waitForHeightArise()

  case class Reserves(wct: Long, usd: Long, waves: Long)
  def reservesOf(node: Node): Reserves = {
    val reserves = matcherNode.reservedBalance(node)
    Reserves(reserves.getOrElse(WctId.toString, 0), reserves.getOrElse(UsdId.toString, 0), reserves.getOrElse("WAVES", 0))
  }

  val (amount, price) = (1000L, PriceConstant)

  "When delete order book" - {
    val buyOrder        = matcherNode.placeOrder(aliceNode, wctUsdPair, BUY, 2 * amount, price).message.id
    val anotherBuyOrder = matcherNode.placeOrder(aliceNode, wctUsdPair, BUY, amount, price).message.id

    val submitted = matcherNode.placeOrder(bobNode, wctUsdPair, SELL, amount, price).message.id

    val sellOrder = matcherNode.placeOrder(bobNode, wctUsdPair, SELL, amount, 2 * price).message.id

    matcherNode.waitOrderStatus(wctUsdPair, buyOrder, "PartiallyFilled")
    matcherNode.waitOrderStatus(wctUsdPair, submitted, "Filled")

    val (aliceReservesForOnePair, bobReservesForOnePair) = (reservesOf(aliceNode), reservesOf(bobNode))

    val buyOrderForAnotherPair  = matcherNode.placeOrder(aliceNode, wctWavesPair, BUY, amount, price).message.id
    val sellOrderForAnotherPair = matcherNode.placeOrder(bobNode, wctWavesPair, SELL, amount, 2 * price).message.id

    matcherNode.waitOrderStatus(wctWavesPair, buyOrderForAnotherPair, "Accepted")
    matcherNode.waitOrderStatus(wctWavesPair, sellOrderForAnotherPair, "Accepted")

    val (aliceReservesForBothPairs, bobReservesForBothPairs) = (reservesOf(aliceNode), reservesOf(bobNode))

    matcherNode.deleteOrderBook(wctUsdPair)

    "orders by the pair should be canceled" in {
      matcherNode.waitOrderStatus(wctUsdPair, buyOrder, "Cancelled")
      matcherNode.waitOrderStatus(wctUsdPair, anotherBuyOrder, "Cancelled")
      matcherNode.waitOrderStatus(wctUsdPair, sellOrder, "Cancelled")
    }

    "orderbook was really deleted" in {
      val orderBook = matcherNode.orderBook(wctUsdPair)
      orderBook.bids shouldBe empty
      orderBook.asks shouldBe empty
    }

    "reserved balances should be released for the pair" in {
      val (aliceReserves, bobReserves) = (reservesOf(aliceNode), reservesOf(bobNode))
      aliceReserves.usd shouldBe 0
      aliceReserves.waves shouldBe (aliceReservesForBothPairs.waves - aliceReservesForOnePair.waves)
      bobReserves.wct shouldBe (bobReservesForBothPairs.wct - bobReservesForOnePair.wct)
      bobReserves.waves shouldBe (bobReservesForBothPairs.waves - bobReservesForOnePair.waves)
    }

    "it should not affect other pairs and their orders" in {
      matcherNode.orderStatus(buyOrderForAnotherPair, wctWavesPair).status shouldBe "Accepted"
      matcherNode.orderStatus(sellOrderForAnotherPair, wctWavesPair).status shouldBe "Accepted"

      val orderBook = matcherNode.orderBook(wctWavesPair)
      orderBook.bids shouldNot be(empty)
      orderBook.asks shouldNot be(empty)
    }
  }

}

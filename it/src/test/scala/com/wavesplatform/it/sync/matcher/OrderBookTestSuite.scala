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

  case class ReservedBalances(wct: Long, usd: Long, waves: Long)
  def reservedBalancesOf(node: Node): ReservedBalances = {
    val reservedBalances = matcherNode.reservedBalance(node)
    ReservedBalances(
      reservedBalances.getOrElse(WctId.toString, 0),
      reservedBalances.getOrElse(UsdId.toString, 0),
      reservedBalances.getOrElse("WAVES", 0)
    )
  }

  val (amount, price) = (1000L, PriceConstant)

  "When delete order book" - {
    val buyOrder        = matcherNode.placeOrder(aliceNode, wctUsdPair, BUY, 2 * amount, price).message.id
    val anotherBuyOrder = matcherNode.placeOrder(aliceNode, wctUsdPair, BUY, amount, price).message.id

    val submitted = matcherNode.placeOrder(bobNode, wctUsdPair, SELL, amount, price).message.id

    val sellOrder = matcherNode.placeOrder(bobNode, wctUsdPair, SELL, amount, 2 * price).message.id

    matcherNode.waitOrderStatus(wctUsdPair, buyOrder, "PartiallyFilled")
    matcherNode.waitOrderStatus(wctUsdPair, submitted, "Filled")

    val (aliceRBForOnePair, bobRBForOnePair) = (reservedBalancesOf(aliceNode), reservedBalancesOf(bobNode))

    val buyOrderForAnotherPair  = matcherNode.placeOrder(aliceNode, wctWavesPair, BUY, amount, price).message.id
    val sellOrderForAnotherPair = matcherNode.placeOrder(bobNode, wctWavesPair, SELL, amount, 2 * price).message.id

    matcherNode.waitOrderStatus(wctWavesPair, buyOrderForAnotherPair, "Accepted")
    matcherNode.waitOrderStatus(wctWavesPair, sellOrderForAnotherPair, "Accepted")

    val (aliceRBForBothPairs, bobRBForBothPairs) = (reservedBalancesOf(aliceNode), reservedBalancesOf(bobNode))

    val marketStatusBeforeDeletion = matcherNode.marketStatus(wctUsdPair)

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
      val (aliceReservedBalances, bobReservedBalances) = (reservedBalancesOf(aliceNode), reservedBalancesOf(bobNode))
      aliceReservedBalances.usd shouldBe 0
      aliceReservedBalances.waves shouldBe (aliceRBForBothPairs.waves - aliceRBForOnePair.waves)
      bobReservedBalances.wct shouldBe (bobRBForBothPairs.wct - bobRBForOnePair.wct)
      bobReservedBalances.waves shouldBe (bobRBForBothPairs.waves - bobRBForOnePair.waves)
    }

    "it should not affect other pairs and their orders" in {
      matcherNode.orderStatus(buyOrderForAnotherPair, wctWavesPair).status shouldBe "Accepted"
      matcherNode.orderStatus(sellOrderForAnotherPair, wctWavesPair).status shouldBe "Accepted"

      val orderBook = matcherNode.orderBook(wctWavesPair)
      orderBook.bids shouldNot be(empty)
      orderBook.asks shouldNot be(empty)
    }

    "it should not affect market status" in {
      matcherNode.marketStatus(wctUsdPair) shouldEqual marketStatusBeforeDeletion
    }
  }

}

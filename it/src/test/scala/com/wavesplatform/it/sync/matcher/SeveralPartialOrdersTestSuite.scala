package com.wavesplatform.it.sync.matcher

import com.typesafe.config.Config
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.LevelResponse
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.transaction.assets.exchange.OrderType.BUY
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._
import scala.math.BigDecimal.RoundingMode
import com.wavesplatform.it.sync._

class SeveralPartialOrdersTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with NodesFromDocker
    with ReportingTestName {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    matcherNode.waitForTransaction(matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx)).id)
  }

  "Alice and Bob trade WAVES-USD" - {
    val price           = 238
    val buyOrderAmount  = 425532L
    val sellOrderAmount = 840340L

    "place two submitted orders and one counter" in {
      val bobWavesBalanceBefore = matcherNode.accountBalances(bobNode.address)._1

      val bobOrder1   = matcherNode.prepareOrder(bobNode, wavesUsdPair, OrderType.SELL, sellOrderAmount, price)
      val bobOrder1Id = matcherNode.placeOrder(bobOrder1).message.id
      matcherNode.waitOrderStatus(wavesUsdPair, bobOrder1Id, "Accepted", 1.minute)
      matcherNode.reservedBalance(bobNode)("WAVES") shouldBe sellOrderAmount + matcherFee
      matcherNode.tradableBalance(bobNode, wavesUsdPair)("WAVES") shouldBe bobWavesBalanceBefore - (sellOrderAmount + matcherFee)

      val aliceOrder1  = matcherNode.prepareOrder(aliceNode, wavesUsdPair, OrderType.BUY, buyOrderAmount, price)
      val aliceOrderId = matcherNode.placeOrder(aliceOrder1).message.id
      matcherNode.waitOrderStatus(wavesUsdPair, aliceOrderId, "Filled", 1.minute)

      val aliceOrder2   = matcherNode.prepareOrder(aliceNode, wavesUsdPair, OrderType.BUY, buyOrderAmount, price)
      val aliceOrder2Id = matcherNode.placeOrder(aliceOrder2).message.id
      matcherNode.waitOrderStatus(wavesUsdPair, aliceOrder2Id, "Filled", 1.minute)

      matcherNode.waitOrderStatus(wavesUsdPair, bobOrder1Id, "Filled", 1.minute)

      // Each side get fair amount of assets
      val exchangeTx = matcherNode.transactionsByOrder(bobOrder1Id).headOption.getOrElse(fail("Expected an exchange transaction"))
      nodes.waitForHeightAriseAndTxPresent(exchangeTx.id)
      matcherNode.reservedBalance(bobNode) shouldBe empty
      matcherNode.reservedBalance(aliceNode) shouldBe empty

      // Previously cancelled order should not affect new orders
      val orderBook1 = matcherNode.orderBook(wavesUsdPair)
      orderBook1.asks shouldBe empty
      orderBook1.bids shouldBe empty

      val bobOrder2   = matcherNode.prepareOrder(bobNode, wavesUsdPair, OrderType.SELL, sellOrderAmount, price)
      val bobOrder2Id = matcherNode.placeOrder(bobOrder2).message.id
      matcherNode.waitOrderStatus(wavesUsdPair, bobOrder2Id, "Accepted", 1.minute)

      val orderBook2 = matcherNode.orderBook(wavesUsdPair)
      orderBook2.asks shouldBe List(LevelResponse(bobOrder2.amount, bobOrder2.price))
      orderBook2.bids shouldBe empty

      matcherNode.cancelOrder(bobNode, wavesUsdPair, bobOrder2Id)
      matcherNode.waitOrderStatus(wavesUsdPair, bobOrder2Id, "Cancelled", 1.minute)

      matcherNode.reservedBalance(bobNode) shouldBe empty
      matcherNode.reservedBalance(aliceNode) shouldBe empty
    }

    "place one submitted orders and two counter" in {
      val aliceOrder1   = matcherNode.prepareOrder(aliceNode, wavesUsdPair, OrderType.BUY, buyOrderAmount, price)
      val aliceOrder1Id = matcherNode.placeOrder(aliceOrder1).message.id

      val aliceOrder2   = matcherNode.prepareOrder(aliceNode, wavesUsdPair, OrderType.BUY, buyOrderAmount, price)
      val aliceOrder2Id = matcherNode.placeOrder(aliceOrder2).message.id

      val bobOrder1   = matcherNode.prepareOrder(bobNode, wavesUsdPair, OrderType.SELL, sellOrderAmount, price)
      val bobOrder1Id = matcherNode.placeOrder(bobOrder1).message.id

      matcherNode.waitOrderStatus(wavesUsdPair, aliceOrder1Id, "Filled", 1.minute)
      matcherNode.waitOrderStatus(wavesUsdPair, aliceOrder2Id, "Filled", 1.minute)
      matcherNode.waitOrderStatus(wavesUsdPair, bobOrder1Id, "Filled", 1.minute)

      // Each side get fair amount of assets
      val exchangeTxs = matcherNode.transactionsByOrder(bobOrder1Id)
      exchangeTxs should not be empty
      exchangeTxs.map(_.id).foreach(nodes.waitForHeightAriseAndTxPresent)

      matcherNode.reservedBalance(bobNode) shouldBe empty
      matcherNode.reservedBalance(aliceNode) shouldBe empty

      // Previously cancelled order should not affect new orders
      val orderBook1 = matcherNode.orderBook(wavesUsdPair)
      orderBook1.asks shouldBe empty
      orderBook1.bids shouldBe empty

      val bobOrder2   = matcherNode.prepareOrder(bobNode, wavesUsdPair, OrderType.SELL, sellOrderAmount, price)
      val bobOrder2Id = matcherNode.placeOrder(bobOrder2).message.id
      matcherNode.waitOrderStatus(wavesUsdPair, bobOrder2Id, "Accepted", 1.minute)

      val orderBook2 = matcherNode.orderBook(wavesUsdPair)
      orderBook2.asks shouldBe List(LevelResponse(bobOrder2.amount, bobOrder2.price))
      orderBook2.bids shouldBe empty
    }
  }

  def correctAmount(a: Long, price: Long): Long = {
    val settledTotal = (BigDecimal(price) * a / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
    (BigDecimal(settledTotal) / price * Order.PriceConstant).setScale(0, RoundingMode.CEILING).toLong
  }

  def receiveAmount(ot: OrderType, matchAmount: Long, matchPrice: Long): Long =
    if (ot == BUY) correctAmount(matchAmount, matchPrice)
    else {
      (BigInt(matchAmount) * matchPrice / Order.PriceConstant).bigInteger.longValueExact()
    }

}

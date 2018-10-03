package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config}
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.assets.exchange.OrderType.BUY
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._
import scala.math.BigDecimal.RoundingMode
import scala.util.{Try}
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._

class CancelOrderTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with NodesFromDocker with ReportingTestName {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
  matcherNode.signedIssue(createSignedIssueRequest(IssueWctTx))
  nodes.waitForHeightArise()

  "cancel order using api-key" in {
    val orderId = matcherNode.placeOrder(bobNode, wavesUsdPair, OrderType.SELL, 800, 100.waves).message.id
    matcherNode.waitOrderStatus(wavesUsdPair, orderId, "Accepted", 1.minute)

    matcherNode.cancelOrderWithApiKey(orderId)
    matcherNode.waitOrderStatus(wavesUsdPair, orderId, "Cancelled", 1.minute)

    matcherNode.fullOrderHistory(bobNode).filter(_.id == orderId).head.status shouldBe "Cancelled"
    matcherNode.orderHistoryByPair(bobNode, wavesUsdPair).filter(_.id == orderId).head.status shouldBe "Cancelled"
    matcherNode.orderBook(wavesUsdPair).bids shouldBe empty
    matcherNode.orderBook(wavesUsdPair).asks shouldBe empty

    matcherNode.deleteOrder(bobNode, wavesUsdPair, Some(orderId))
    matcherNode.orderStatus(orderId, wavesUsdPair, false).status shouldBe "NotFound"

  }

  "Alice and Bob trade WAVES-USD" - {
    "place usd-waves order" in {
      // Alice wants to sell USD for Waves
      val orderId1      = matcherNode.placeOrder(bobNode, wavesUsdPair, OrderType.SELL, 800, 100.waves).message.id
      val orderId2      = matcherNode.placeOrder(bobNode, wavesUsdPair, OrderType.SELL, 700, 100.waves).message.id
      val bobSellOrder3 = matcherNode.placeOrder(bobNode, wavesUsdPair, OrderType.SELL, 600, 100.waves).message.id

      matcherNode.fullOrderHistory(aliceNode)
      matcherNode.fullOrderHistory(bobNode)

      matcherNode.waitOrderStatus(wavesUsdPair, bobSellOrder3, "Accepted", 1.minute)

      val aliceOrder = matcherNode.prepareOrder(aliceNode, wavesUsdPair, OrderType.BUY, 800, 0.00125.waves)
      matcherNode.placeOrder(aliceOrder).message.id

      Thread.sleep(2000)
      matcherNode.fullOrderHistory(aliceNode)
      val orders = matcherNode.fullOrderHistory(bobNode)
      for (orderId <- Seq(orderId1, orderId2)) {
        orders.filter(_.id == orderId).head.status shouldBe "Accepted"
      }
    }

  }

  def correctAmount(a: Long, price: Long): Long = {
    val min = (BigDecimal(Order.PriceConstant) / price).setScale(0, RoundingMode.CEILING)
    if (min > 0)
      Try(((BigDecimal(a) / min).toBigInt() * min.toBigInt()).bigInteger.longValueExact()).getOrElse(Long.MaxValue)
    else
      a
  }

  def receiveAmount(ot: OrderType, matchPrice: Long, matchAmount: Long): Long =
    if (ot == BUY) correctAmount(matchAmount, matchPrice)
    else {
      (BigInt(matchAmount) * matchPrice / Order.PriceConstant).bigInteger.longValueExact()
    }

}

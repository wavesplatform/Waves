package com.wavesplatform.it.sync.matcher

import com.typesafe.config.Config
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._

class CancelOrderTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with NodesFromDocker with ReportingTestName {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def bobNode = nodes(2)

  private val wavesBtcPair = AssetPair(None, Some(BtcId))

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
    matcherNode.signedIssue(createSignedIssueRequest(IssueBtcTx))
    nodes.waitForHeightArise()
  }

  "Order can be canceled" - {
    "by sender" in {
      val orderId = matcherNode.placeOrder(bobNode, wavesUsdPair, OrderType.SELL, 100.waves, 800).message.id
      matcherNode.waitOrderStatus(wavesUsdPair, orderId, "Accepted", 1.minute)
      matcherNode.cancelOrder(bobNode, wavesUsdPair, orderId)
      matcherNode.waitOrderStatus(wavesUsdPair, orderId, "Cancelled", 1.minute)
      matcherNode.orderHistoryByPair(bobNode, wavesUsdPair).collectFirst {
        case o if o.id == orderId => o.status shouldEqual "Cancelled"
      }
    }
    "with API key" in {
      val orderId = matcherNode.placeOrder(bobNode, wavesUsdPair, OrderType.SELL, 100.waves, 800).message.id
      matcherNode.waitOrderStatus(wavesUsdPair, orderId, "Accepted", 1.minute)

      matcherNode.cancelOrderWithApiKey(orderId)
      matcherNode.waitOrderStatus(wavesUsdPair, orderId, "Cancelled", 1.minute)

      matcherNode.fullOrderHistory(bobNode).filter(_.id == orderId).head.status shouldBe "Cancelled"
      matcherNode.orderHistoryByPair(bobNode, wavesUsdPair).filter(_.id == orderId).head.status shouldBe "Cancelled"

      val orderBook = matcherNode.orderBook(wavesUsdPair)
      orderBook.bids shouldBe empty
      orderBook.asks shouldBe empty
    }
  }

  "Cancel is rejected" - {
    "when request sender is not the sender of and order" in {
      val orderId = matcherNode.placeOrder(bobNode, wavesUsdPair, OrderType.SELL, 100.waves, 800).message.id
      matcherNode.waitOrderStatus(wavesUsdPair, orderId, "Accepted", 1.minute)

      matcherNode.expectCancelRejected(matcherNode.privateKey, wavesUsdPair, orderId)
    }
  }

  "Batch cancel" - {
    "works for" - {
      "all orders placed by an address" in {
        val usdOrderIds = 1 to 5 map { i =>
          matcherNode.placeOrder(bobNode, wavesUsdPair, OrderType.SELL, 100.waves + i, 400).message.id
        }

        val btcOrderIds = 1 to 5 map { i =>
          matcherNode.placeOrder(bobNode, wavesBtcPair, OrderType.BUY, 100.waves + i, 400).message.id
        }

        (usdOrderIds ++ btcOrderIds).foreach(id => matcherNode.waitOrderStatus(wavesUsdPair, id, "Accepted"))

        matcherNode.cancelAllOrders(bobNode)

        (usdOrderIds ++ btcOrderIds).foreach(id => matcherNode.waitOrderStatus(wavesUsdPair, id, "Cancelled"))
      }
      "a pair" in {
        val usdOrderIds = 1 to 5 map { i =>
          matcherNode.placeOrder(bobNode, wavesUsdPair, OrderType.SELL, 100.waves + i, 400).message.id
        }

        val btcOrderIds = 1 to 5 map { i =>
          matcherNode.placeOrder(bobNode, wavesBtcPair, OrderType.BUY, 100.waves + i, 400).message.id
        }

        (usdOrderIds ++ btcOrderIds).foreach(id => matcherNode.waitOrderStatus(wavesUsdPair, id, "Accepted"))

        matcherNode.cancelOrdersForPair(bobNode, wavesBtcPair)

        btcOrderIds.foreach(id => matcherNode.waitOrderStatus(wavesUsdPair, id, "Cancelled"))
        usdOrderIds.foreach(id => matcherNode.waitOrderStatus(wavesUsdPair, id, "Accepted"))
      }
    }
  }
}

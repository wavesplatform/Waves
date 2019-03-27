package com.wavesplatform.it.sync

import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}

import scala.concurrent.duration._

class CancelOrderTestSuite extends MatcherSuiteBase {
  private val wavesBtcPair = AssetPair(Waves, IssuedAsset(BtcId))

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val xs = Seq(IssueUsdTx, IssueBtcTx).map(_.json()).map(node.signedBroadcast(_))
    xs.foreach(tx => node.waitForTransaction(tx.id))
  }

  "Order can be canceled" - {
    "by sender" in {
      val orderId = node.placeOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves, 800, matcherFee).message.id
      node.waitOrderStatus(wavesUsdPair, orderId, "Accepted", 1.minute)

      node.cancelOrder(bob, wavesUsdPair, orderId)
      node.waitOrderStatus(wavesUsdPair, orderId, "Cancelled", 1.minute)

      node.orderHistoryByPair(bob, wavesUsdPair).collectFirst {
        case o if o.id == orderId => o.status shouldEqual "Cancelled"
      }
    }
    "with API key" in {
      val orderId = node.placeOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves, 800, matcherFee).message.id
      node.waitOrderStatus(wavesUsdPair, orderId, "Accepted", 1.minute)

      node.cancelOrderWithApiKey(orderId)
      node.waitOrderStatus(wavesUsdPair, orderId, "Cancelled", 1.minute)

      node.fullOrderHistory(bob).filter(_.id == orderId).head.status shouldBe "Cancelled"
      node.orderHistoryByPair(bob, wavesUsdPair).filter(_.id == orderId).head.status shouldBe "Cancelled"

      val orderBook = node.orderBook(wavesUsdPair)
      orderBook.bids shouldBe empty
      orderBook.asks shouldBe empty
    }
  }

  "Cancel is rejected" - {
    "when request sender is not the sender of and order" in {
      val orderId = node.placeOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves, 800, matcherFee).message.id
      node.waitOrderStatus(wavesUsdPair, orderId, "Accepted", 1.minute)

      node.expectCancelRejected(matcher, wavesUsdPair, orderId)

      // Cleanup
      node.cancelOrder(bob, wavesUsdPair, orderId)
      node.waitOrderStatus(wavesUsdPair, orderId, "Cancelled")
    }
  }

  "Batch cancel" - {
    "works for" - {
      "all orders placed by an address" in {
        node.fullOrderHistory(bob)

        val usdOrderIds = 1 to 5 map { i =>
          node.placeOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves + i, 400, matcherFee).message.id
        }

        node.assetBalance(bob.toAddress.stringRepr, BtcId.base58)

        val btcOrderIds = 1 to 5 map { i =>
          node.placeOrder(bob, wavesBtcPair, OrderType.BUY, 100.waves + i, 400, matcherFee).message.id
        }

        (usdOrderIds ++ btcOrderIds).foreach(id => node.waitOrderStatus(wavesUsdPair, id, "Accepted"))

        node.cancelAllOrders(bob)

        (usdOrderIds ++ btcOrderIds).foreach(id => node.waitOrderStatus(wavesUsdPair, id, "Cancelled"))
      }

      "a pair" in {
        val usdOrderIds = 1 to 5 map { i =>
          node.placeOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves + i, 400, matcherFee).message.id
        }

        val btcOrderIds = 1 to 5 map { i =>
          node.placeOrder(bob, wavesBtcPair, OrderType.BUY, 100.waves + i, 400, matcherFee).message.id
        }

        (usdOrderIds ++ btcOrderIds).foreach(id => node.waitOrderStatus(wavesUsdPair, id, "Accepted"))

        node.cancelOrdersForPair(bob, wavesBtcPair)

        btcOrderIds.foreach(id => node.waitOrderStatus(wavesUsdPair, id, "Cancelled"))
        usdOrderIds.foreach(id => node.waitOrderStatus(wavesUsdPair, id, "Accepted"))
      }
    }
  }
}

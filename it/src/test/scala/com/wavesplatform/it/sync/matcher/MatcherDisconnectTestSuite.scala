package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it._
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.state.ByteStr
import com.wavesplatform.it.{TransferSending}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Random

class MatcherDisconnectTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with ReportingTestName
    with NodesFromDocker
    with TransferSending {

  import MatcherDisconnectTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs
  private def matcherNode                         = nodes.head
  private def aliceNode                           = nodes(1)
  private val nodeAddresses                       = nodeConfigs.map(_.getString("address")).toSet

  "check order execution" - {
    // Alice issues new asset
    val aliceAsset =
      aliceNode.issue(aliceNode.address, "DisconnectCoin", "Alice's coin for disconnect tests", AssetQuantity, 0, reissuable = false, 100000000L).id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)
    val aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    // check assets's balances
    aliceNode.assertAssetBalance(aliceNode.address, aliceAsset, AssetQuantity)
    aliceNode.assertAssetBalance(matcherNode.address, aliceAsset, 0)

    "matcher responds with Public key" in {
      matcherNode.matcherGet("/matcher").getResponseBody.stripPrefix("\"").stripSuffix("\"") shouldBe matcherNode.publicKeyStr
    }

    "make order and after matcher's restart try to cancel it" in {
      // Alice places sell order
      val aliceOrder = matcherNode
        .placeOrder(aliceNode, aliceWavesPair, OrderType.SELL, 2.waves * Order.PriceConstant, 500, 5.minutes)

      aliceOrder.status shouldBe "OrderAccepted"

      val orderId = aliceOrder.message.id

      // check order status
      matcherNode.orderStatus(orderId, aliceWavesPair).status shouldBe "Accepted"

      // check that order is correct
      val orders = matcherNode.orderBook(aliceWavesPair)
      orders.asks.head.amount shouldBe 500
      orders.asks.head.price shouldBe 2.waves * Order.PriceConstant

      // sell order should be in the aliceNode orderbook
      matcherNode.orderHistory(aliceNode).head.status shouldBe "Accepted"

      val height = nodes.map(_.height).max
      Await.result(processRequests(generateTransfersToRandomAddresses(4000, nodeAddresses)), 2.minutes)
      aliceNode.utxSize
      matcherNode.utxSize
      nodes.waitForHeight(height + 2)

      // disconnect matcher node from network
      docker.disconnectFromNetwork(dockerNodes().head)
      Thread.sleep(90.seconds.toMillis)

      // connect back matcher's node
      docker.connectToNetwork(Seq(dockerNodes().head))

      val height2 = nodes.map(_.height).max
      Await.result(processRequests(generateTransfersToRandomAddresses(4000, nodeAddresses)), 2.minutes)
      aliceNode.utxSize
      matcherNode.utxSize
      nodes.waitForHeight(height2 + 5)

      val aliceSecondOrder = matcherNode
        .placeOrder(aliceNode, aliceWavesPair, OrderType.SELL, 2.waves * Order.PriceConstant, 500, 5.minutes)

      aliceSecondOrder.status shouldBe "OrderAccepted"

      val orders1 = matcherNode.orderBook(aliceWavesPair)
      orders1.asks.head.amount shouldBe 1000
      orders1.asks.head.price shouldBe 2.waves * Order.PriceConstant
      aliceNode.utxSize
      matcherNode.utxSize

      nodes.waitForHeightArise()

      // try to cancel order
      val cancel = matcherNode.cancelOrder(aliceNode, aliceWavesPair, Some(orderId))
      cancel.status should be("OrderCanceled")
      val orders2 = matcherNode.orderBook(aliceWavesPair)
      orders2.asks.head.amount shouldBe 500

      matcherNode.orderStatus(orderId, aliceWavesPair).status should be("Cancelled")
      matcherNode.orderHistory(aliceNode).head.status shouldBe "Accepted"
      //    matcherNode.orderHistory(aliceNode).filter(o => o.id == orderId) should be("Cancelled")
      matcherNode.orderHistory(aliceNode)

    }
  }

}

object MatcherDisconnectTestSuite {
  val ForbiddenAssetId = "FdbnAsset"

  import NodeConfigs.Default

  private val matcherConfig = ConfigFactory.parseString(s"""
                                                           |waves {
                                                           |  matcher {
                                                           |    enable = yes
                                                           |    account = 3HmFkAoQRs4Y3PE2uR6ohN7wS4VqPBGKv7k
                                                           |    bind-address = "0.0.0.0"
                                                           |    order-match-tx-fee = 300000
                                                           |    blacklisted-assets = [$ForbiddenAssetId]
                                                           |    order-cleanup-interval = 20s
                                                           |  }
                                                           |  rest-api {
                                                           |    enable = yes
                                                           |    api-key-hash = 7L6GpLHhA5KyJTAVc8WFHwEcyTY8fC8rRbyMCiFnM4i
                                                           |  }
                                                           |  miner.enable=no
                                                           |}""".stripMargin)

  private val nonGeneratingPeersConfig = ConfigFactory.parseString(
    """waves {
      | matcher.order-cleanup-interval = 30s
      | miner.enable=no
      |}""".stripMargin
  )

  val AssetQuantity: Long = 1000

  val MatcherFee: Long     = 300000
  val TransactionFee: Long = 300000

  private val Configs: Seq[Config] = {
    val notMatchingNodes = Random.shuffle(Default.init).take(3)
    Seq(matcherConfig.withFallback(Default.last), notMatchingNodes.head) ++
      notMatchingNodes.tail.map(nonGeneratingPeersConfig.withFallback)
  }
}

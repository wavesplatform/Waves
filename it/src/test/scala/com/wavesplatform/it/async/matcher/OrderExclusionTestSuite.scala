package com.wavesplatform.it.async.matcher

import com.google.common.primitives.Longs
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.crypto
import com.wavesplatform.it._
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.state2.ByteStr
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Random

class OrderExclusionTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with ReportingTestName
    with NodesFromDocker
    with MatcherUtils {

  import OrderExclusionTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private var aliceSell1 = ""

  private var aliceAsset: String        = ""
  private var aliceWavesPair: AssetPair = AssetPair(None, None)

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    // Alice issues new asset
    aliceAsset = issueAsset(aliceNode, "AliceCoin", AssetQuantity)
    aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    // Wait for balance on Alice's account
    waitForAssetBalance(aliceNode, aliceAsset, AssetQuantity)
    waitForAssetBalance(matcherNode, aliceAsset, 0)
  }

  "matcher should respond with Public key" in {
    Await.result(matcherNode.matcherGet("/matcher"), 1.minute).getResponseBody.stripPrefix("\"").stripSuffix("\"") shouldBe matcherNode.publicKeyStr
  }

  "sell order could be placed" in {
    // Alice places sell order
    val (id, status) =
      matcherPlaceOrder(matcherNode,
                        prepareOrder(aliceNode, matcherNode, aliceWavesPair, OrderType.SELL, 2 * Waves * Order.PriceConstant, 500, 70.seconds))
    status shouldBe "OrderAccepted"
    aliceSell1 = id
    // Alice checks that the order in order book
    matcherCheckOrderStatus(matcherNode, aliceAsset, id) shouldBe "Accepted"
    waitForOrderStatus(matcherNode, aliceAsset, id, "Accepted", 1.minute)
    matcherCheckOrderStatus(matcherNode, aliceAsset, id) shouldBe "Accepted"

    // Alice check that order is correct
    val orders = matcherGetOrderBook(matcherNode, aliceAsset)
    orders.asks.head.amount shouldBe 500
    orders.asks.head.price shouldBe 2 * Waves * Order.PriceConstant
  }

  "sell order should be in the aliceNode orderbook" in {
    orderStatus(aliceNode) shouldBe "Accepted"
  }

  "wait for expiration" in {
    waitForOrderStatus(matcherNode, aliceAsset, aliceSell1, "Cancelled", 3.minutes)
    orderStatus(aliceNode) shouldBe "Cancelled"
  }

  private def orderStatus(node: Node) = {
    val ts         = System.currentTimeMillis()
    val privateKey = aliceNode.privateKey

    val pk        = node.publicKey.publicKey
    val signature = ByteStr(crypto.sign(privateKey, pk ++ Longs.toByteArray(ts)))

    val orderhistory = Await.result(matcherNode.getOrderbookByPublicKey(node.publicKeyStr, ts, signature), 1.minute)
    orderhistory.seq(0).status
  }

}

object OrderExclusionTestSuite {
  val ForbiddenAssetId = "FdbnAsset"

  import NodeConfigs.Default

  private val matcherConfig = ConfigFactory.parseString(s"""
       |waves.matcher {
       |  enable=yes
       |  account="3Hm3LGoNPmw1VTZ3eRA2pAfeQPhnaBm6YFC"
       |  bind-address="0.0.0.0"
       |  order-match-tx-fee = 300000
       |  blacklisted-assets = [$ForbiddenAssetId]
       |  order-cleanup-interval = 20s
       |}
       |waves.rest-api {
       |    enable = yes
       |    api-key-hash = 7L6GpLHhA5KyJTAVc8WFHwEcyTY8fC8rRbyMCiFnM4i
       |}
       |waves.miner.enable=no
      """.stripMargin)

  private val nonGeneratingPeersConfig = ConfigFactory.parseString(
    """
      |waves.matcher {
      | order-cleanup-interval = 30s
      |}
      |waves.miner.enable=no
    """.stripMargin
  )

  val AssetQuantity: Long = 1000

  val MatcherFee: Long     = 300000
  val TransactionFee: Long = 300000

  val Waves: Long = 100000000L

  val Configs: Seq[Config] = Seq(matcherConfig.withFallback(Default.head)) ++
    Random.shuffle(Default.tail.init).take(2).map(nonGeneratingPeersConfig.withFallback(_)) ++
    Seq(Default.last)

}

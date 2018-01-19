package com.wavesplatform.it.matcher

import com.google.common.primitives.Longs
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it._
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.api._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.state2.ByteStr
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class OrderExclusionTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure
  with ReportingTestName with OrderGenerator with NodesFromDocker {

  import OrderExclusionTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private var aliceSell1 = ""

  private var aliceAsset: String = ""
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
    Await.result(matcherNode.matcherGet("/matcher"), 1.minute)
      .getResponseBody.stripPrefix("\"").stripSuffix("\"") shouldBe matcherNode.publicKey
  }

  "sell order could be placed" in {
    // Alice places sell order
    val (id, status) = matcherPlaceOrder(
      prepareOrder(aliceNode, matcherNode, aliceWavesPair, OrderType.SELL, 2 * Waves * Order.PriceConstant, 500, 70.seconds))
    status shouldBe "OrderAccepted"
    aliceSell1 = id
    // Alice checks that the order in order book
    matcherCheckOrderStatus(id) shouldBe "Accepted"

    // Alice check that order is correct
    val orders = matcherGetOrderBook()
    orders.asks.head.amount shouldBe 500
    orders.asks.head.price shouldBe 2 * Waves * Order.PriceConstant
  }

  "sell order should be in the aliceNode orderbook" in {
    orderStatus(aliceNode) shouldBe "Accepted"
  }

  "wait for expiration" in {
    waitForOrderStatus(aliceAsset, aliceSell1, "Cancelled", 3.minutes)
    orderStatus(aliceNode) shouldBe "Cancelled"
  }

  private def orderStatus(node: Node) = {
    val ts = System.currentTimeMillis()
    val privateKey = aliceNode.privateKey

    val pk = node.publicKey.publicKey
    val signature = ByteStr(EllipticCurveImpl.sign(privateKey, pk ++ Longs.toByteArray(ts)))

    val orderhistory = Await.result(matcherNode.getOrderbookByPublicKey(node.publicKey.toString, ts, signature), 1.minute)
    orderhistory.seq(0).status
  }

  private def waitForAssetBalance(node: Node, asset: String, expectedBalance: Long): Unit =
    Await.result(
      node.waitFor[AssetBalance](s"asset($asset) balance of ${node.address} >= $expectedBalance")
        (_.assetBalance(node.address, asset),
          _.balance >= expectedBalance, 5.seconds),
      3.minute
    )


  private def matcherPlaceOrder(order: Order): (String, String) = {
    val futureResult = matcherNode.placeOrder(order)

    val result = Await.result(futureResult, 1.minute)

    (result.message.id, result.status)
  }

  private def issueAsset(node: Node, name: String, amount: Long): String = {
    val description = "asset for integration tests of matcher"
    val fee = 100000000L
    val futureIssueTransaction: Future[Transaction] = for {
      a <- node.issueAsset(node.address, name, description, amount, 0, fee, reissuable = false)
    } yield a

    val issueTransaction = Await.result(futureIssueTransaction, 1.minute)

    issueTransaction.id
  }

  def matcherCheckOrderStatus(id: String): String = {
    val futureResult = matcherNode.getOrderStatus(aliceAsset, id)

    val response = Await.result(futureResult, 1.minute)

    response.status
  }

  def waitForOrderStatus(asset: String, orderId: String, expectedStatus: String, timeout: Duration): Unit = Await.result(
    matcherNode.waitFor[MatcherStatusResponse](s"order(asset=$asset, orderId=$orderId) status == $expectedStatus")
      (_.getOrderStatus(asset, orderId),
        _.status == expectedStatus, 5.seconds),
    timeout
  )

  def matcherGetOrderBook(): OrderBookResponse = {
    val futureResult = matcherNode.getOrderBook(aliceAsset)

    val result = Await.result(futureResult, 1.minute)

    result
  }

}

object OrderExclusionTestSuite {
  val ForbiddenAssetId = "FdbnAsset"

  import NodeConfigs.Default

  private val matcherConfig = ConfigFactory.parseString(
    s"""
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

  val MatcherFee: Long = 300000
  val TransactionFee: Long = 300000

  val Waves: Long = 100000000L

  val Configs: Seq[Config] = Seq(matcherConfig.withFallback(Default.head)) ++
    Random.shuffle(Default.tail.init).take(2).map(nonGeneratingPeersConfig.withFallback(_)) ++
    Seq(Default.last)


}

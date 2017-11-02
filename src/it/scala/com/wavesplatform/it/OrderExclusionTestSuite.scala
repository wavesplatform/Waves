package com.wavesplatform.it


import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.NodeApi.{AssetBalance, MatcherStatusResponse, OrderBookResponse, Transaction}
import com.wavesplatform.matcher.api.CancelOrderRequest
import com.wavesplatform.state2.ByteStr
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class OrderExclusionTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with ReportingTestName {

  import OrderExclusionTestSuite._

  private val docker = Docker(getClass)

  override val nodes = Configs.map(docker.startNode)

  private val matcherNode = nodes.head
  private val aliceNode = nodes(1)
  private val bobNode = nodes(2)

  private var matcherBalance = (0L, 0L)
  private var aliceBalance = (0L, 0L)
  private var bobBalance = (0L, 0L)

  private var aliceSell1 = ""
  private var bobBuy1 = ""

  private var aliceAsset: String = ""
  private var aliceWavesPair: AssetPair = AssetPair(None, None)

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    // Store initial balances of participants
    matcherBalance = getBalance(matcherNode)
    bobBalance = getBalance(bobNode)

    // Alice issues new asset
    aliceAsset = issueAsset(aliceNode, "AliceCoin", AssetQuantity)
    aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    // Wait for balance on Alice's account
    waitForAssetBalance(aliceNode, aliceAsset, AssetQuantity)
    waitForAssetBalance(matcherNode, aliceAsset, 0)
    waitForAssetBalance(bobNode, aliceAsset, 0)

    // Alice spent 1 Wave to issue the asset
    aliceBalance = getBalance(aliceNode)
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }

  "matcher should respond with Public key" in {
    Await.result(matcherNode.matcherGet("/matcher"), 1.minute)
      .getResponseBody.stripPrefix("\"").stripSuffix("\"") shouldBe matcherNode.publicKey
  }

  "sell order could be placed" in {
    // Alice places sell order
    val (id, status) = matcherPlaceOrder(
      prepareOrder(aliceNode, aliceWavesPair, OrderType.SELL, 2 * Waves * Order.PriceConstant, 500))
    status shouldBe "OrderAccepted"
    aliceSell1 = id
    // Alice checks that the order in order book
    matcherCheckOrderStatus(id) shouldBe "Accepted"

    // Alice check that order is correct
    val orders = matcherGetOrderBook()
    orders.asks.head.amount shouldBe 500
    orders.asks.head.price shouldBe 2 * Waves * Order.PriceConstant
  }

  "wait for expiration" in {

    waitForOrderStatus(aliceAsset, aliceSell1, "Cancelled", 2.minutes)

  }


  private def waitForAssetBalance(node: Node, asset: String, expectedBalance: Long): Unit =
    Await.result(
      node.waitFor[AssetBalance](_.assetBalance(node.address, asset), _.balance >= expectedBalance, 5.seconds),
      3.minute
    )

  private def getBalance(node: Node): (Long, Long) = {
    val initialHeight = Await.result(node.height, 1.minute)
    Await.result(node.waitForHeight(initialHeight + 2), 2.minute)

    val balance = Await.result(node.balance(node.address), 1.minute).balance
    val height = Await.result(node.height, 1.minute)

    (balance, height)
  }

  private def prepareOrder(node: Node, pair: AssetPair, orderType: OrderType, price: Long, amount: Long): Order = {
    val creationTime = System.currentTimeMillis()
    val timeToLive = creationTime + Order.MaxLiveTime - 1000

    val privateKey = PrivateKeyAccount(Base58.decode(node.accountSeed).get)
    val matcherPublicKey = PublicKeyAccount(Base58.decode(matcherNode.publicKey).get)

    Order(privateKey, matcherPublicKey, pair, orderType, price, amount, creationTime, timeToLive, MatcherFee)
  }

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

  private def matcherExpectOrderPlacementRejected(order: Order, expectedStatusCode: Int, expectedStatus: String): Boolean = {
    val futureResult = matcherNode.expectIncorrectOrderPlacement(order, expectedStatusCode, expectedStatus)

    Await.result(futureResult, 1.minute)
  }

  def matcherCheckOrderStatus(id: String): String = {
    val futureResult = matcherNode.getOrderStatus(aliceAsset, id)

    val response = Await.result(futureResult, 1.minute)

    response.status
  }

  def waitForOrderStatus(asset: String, orderId: String, expectedStatus: String, timeout: Duration): Unit = Await.result(
    matcherNode.waitFor[MatcherStatusResponse](_.getOrderStatus(asset, orderId), _.status == expectedStatus, 5.seconds),
    timeout
  )

  def matcherGetOrderBook(): OrderBookResponse = {
    val futureResult = matcherNode.getOrderBook(aliceAsset)

    val result = Await.result(futureResult, 1.minute)

    result
  }

  private def matcherCancelOrder(node: Node, pair: AssetPair, orderId: String): String = {
    val privateKey = PrivateKeyAccount(Base58.decode(node.accountSeed).get)
    val publicKey = PublicKeyAccount(Base58.decode(node.publicKey).get)
    val request = CancelOrderRequest(publicKey, Base58.decode(orderId).get, Array.emptyByteArray)
    val signedRequest = CancelOrderRequest.sign(request, privateKey)
    val futureResult = matcherNode.cancelOrder(pair.amountAssetStr, pair.priceAssetStr, signedRequest)

    val result = Await.result(futureResult, 1.minute)

    result.status
  }

}

object OrderExclusionTestSuite {
  val ForbiddenAssetId = "FdbnAsset"

  private val dockerConfigs = Docker.NodeConfigs.getConfigList("nodes").asScala

  private val matcherConfig = ConfigFactory.parseString(
    s"""
       |waves.matcher {
       |  enable=yes
       |  account="3Hm3LGoNPmw1VTZ3eRA2pAfeQPhnaBm6YFC"
       |  bind-address="0.0.0.0"
       |  order-match-tx-fee = 300000
       |  blacklisted-assets = [$ForbiddenAssetId]
       |  order-cleanup-interval = 90s
       |}
       |waves.miner.enable=no
      """.stripMargin)

  private val nonGeneratingPeersConfig = ConfigFactory.parseString(
    """
      |waves.miner.enable=no
    """.stripMargin
  )

  val AssetQuantity: Long = 1000

  val MatcherFee: Long = 300000
  val TransactionFee: Long = 300000

  val Waves: Long = 100000000L

  val Configs: Seq[Config] = Seq(matcherConfig.withFallback(dockerConfigs.head)) ++
    Random.shuffle(dockerConfigs.tail.init).take(2).map(nonGeneratingPeersConfig.withFallback(_)) ++
    Seq(dockerConfigs.last)


}

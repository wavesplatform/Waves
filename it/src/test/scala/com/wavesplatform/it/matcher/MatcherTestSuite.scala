package com.wavesplatform.it
package matcher

import com.google.common.primitives.Longs
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.api._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.matcher.api.CancelOrderRequest
import com.wavesplatform.state2.ByteStr
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}
import play.api.libs.json.Json.parse
import play.api.libs.json.{JsArray, JsNumber, JsString}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class MatcherTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with NodesFromDocker
  with ReportingTestName {

  import MatcherTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  private var matcherBalance = (0L, 0L)
  private var aliceBalance = (0L, 0L)
  private var bobBalance = (0L, 0L)

  private val aliceSellAmount = 500
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

  "matcher should respond with Public key" in {
    Await.result(matcherNode.matcherGet("/matcher"), 1.minute)
      .getResponseBody.stripPrefix("\"").stripSuffix("\"") shouldBe matcherNode.publicKey
  }

  "sell order could be placed" in {
    // Alice places sell order
    val (id, status) = matcherPlaceOrder(
      prepareOrder(aliceNode, aliceWavesPair, OrderType.SELL, 2 * Waves * Order.PriceConstant, aliceSellAmount))
    status shouldBe "OrderAccepted"
    aliceSell1 = id
    // Alice checks that the order in order book
    matcherCheckOrderStatus(id) shouldBe "Accepted"

    // Alice check that order is correct
    val orders = matcherGetOrderBook()
    orders.asks.head.amount shouldBe aliceSellAmount
    orders.asks.head.price shouldBe 2 * Waves * Order.PriceConstant
  }

  "froze amount should be listed via matcherBalance REST endpoint" in {
    val ts = System.currentTimeMillis()
    val privateKey = aliceNode.privateKey
    val signature = Base58.encode(EllipticCurveImpl.sign(privateKey, privateKey.publicKey ++ Longs.toByteArray(ts)))

    val json = parse(Await.result(matcherNode.matcherGet(s"/matcher/matcherBalance/${aliceNode.publicKey}", _
      .addHeader("Timestamp", ts)
      .addHeader("Signature", signature)), 1.minute).getResponseBody)

    (json \ aliceAsset).get shouldBe JsNumber(aliceSellAmount)
  }

  "and should be listed by trader's publiс key via REST" in {
    val ts = System.currentTimeMillis()
    val privateKey = aliceNode.privateKey
    val signature = Base58.encode(EllipticCurveImpl.sign(privateKey, privateKey.publicKey ++ Longs.toByteArray(ts)))

    val json = parse(Await.result(matcherNode.matcherGet(s"/matcher/orderbook/${aliceNode.publicKey}", _
      .addHeader("Timestamp", ts)
      .addHeader("Signature", signature)), 1.minute).getResponseBody)

    (json.as[JsArray].value.head \ "id").get shouldBe JsString(aliceSell1)
  }

  "and should match with buy order" in {
    // Bob places a buy order
    val (id, status) = matcherPlaceOrder(
      prepareOrder(bobNode, aliceWavesPair, OrderType.BUY, 2 * Waves * Order.PriceConstant, 200))
    bobBuy1 = id
    status shouldBe "OrderAccepted"

    waitForOrderStatus(aliceAsset, aliceSell1, "PartiallyFilled")

    // Bob waits for order to fill
    waitForOrderStatus(aliceAsset, bobBuy1, "Filled")

    // Bob checks that asset on his balance
    waitForAssetBalance(bobNode, aliceAsset, 200)

    // Alice checks that part of her order still in the order book
    val orders = matcherGetOrderBook()
    orders.asks.head.amount shouldBe 300
    orders.asks.head.price shouldBe 2 * Waves * Order.PriceConstant

    // Alice checks that she sold some assets
    waitForAssetBalance(aliceNode, aliceAsset, 800)

    // Bob checks that he spent some Waves
    val updatedBobBalance = getBalance(bobNode)
    updatedBobBalance._1 shouldBe (bobBalance._1 - 2 * Waves * 200 - MatcherFee)
    bobBalance = updatedBobBalance

    // Alice checks that she received some Waves
    val updatedAliceBalance = getBalance(aliceNode)
    updatedAliceBalance._1 shouldBe (aliceBalance._1 + 2 * Waves * 200 - (MatcherFee * 200.0 / 500.0).toLong)
    aliceBalance = updatedAliceBalance

    // Matcher checks that it earn fees
    val updatedMatcherBalance = getBalance(matcherNode)
    updatedMatcherBalance._1 shouldBe (matcherBalance._1 + MatcherFee + (MatcherFee * 200.0 / 500.0).toLong - TransactionFee)
    matcherBalance = updatedMatcherBalance
  }

  "submitting sell orders should check availability of asset" in {
    // Bob trying to place order on more assets than he has - order rejected
    val badOrder = prepareOrder(bobNode, aliceWavesPair, OrderType.SELL, (19.0 * Waves / 10.0 * Order.PriceConstant).toLong, 300)
    val error = matcherExpectOrderPlacementRejected(badOrder, 400, "OrderRejected")
    error should be(true)

    // Bob places order on available amount of assets - order accepted
    val goodOrder = prepareOrder(bobNode, aliceWavesPair, OrderType.SELL, (19.0 * Waves / 10.0 * Order.PriceConstant).toLong, 150)
    val (_, status) = matcherPlaceOrder(goodOrder)
    status should be("OrderAccepted")

    // Bob checks that the order in the order book
    val orders = matcherGetOrderBook()
    orders.asks should contain(LevelResponse(19 * Waves / 10 * Order.PriceConstant, 150))
  }

  "buy order should match on few price levels" in {
    // Alice places a buy order
    val order = prepareOrder(aliceNode, aliceWavesPair, OrderType.BUY, (21.0 * Waves / 10.0 * Order.PriceConstant).toLong, 350)
    val (id, status) = matcherPlaceOrder(order)
    status should be("OrderAccepted")

    // Where were 2 sells that should fulfill placed order
    waitForOrderStatus(aliceAsset, id, "Filled")

    // Check balances
    waitForAssetBalance(aliceNode, aliceAsset, 950)
    waitForAssetBalance(bobNode, aliceAsset, 50)

    val updatedMatcherBalance = getBalance(matcherNode)
    updatedMatcherBalance._1 should be(matcherBalance._1 - 2 * TransactionFee + MatcherFee + (MatcherFee * 150.0 / 350.0).toLong + (MatcherFee * 200.0 / 350.0).toLong + (MatcherFee * 200.0 / 500.0).toLong)
    matcherBalance = updatedMatcherBalance

    val updatedBobBalance = getBalance(bobNode)
    updatedBobBalance._1 should be(bobBalance._1 - MatcherFee + 150 * (19.0 * Waves / 10.0).toLong)
    bobBalance = updatedBobBalance

    val updatedAliceBalance = getBalance(aliceNode)
    updatedAliceBalance._1 should be(aliceBalance._1 - (MatcherFee * 200.0 / 350.0).toLong - (MatcherFee * 150.0 / 350.0).toLong - (MatcherFee * 200.0 / 500.0).toLong - (19.0 * Waves / 10.0).toLong * 150)
    aliceBalance = updatedAliceBalance
  }

  "order could be canceled and resubmitted again" in {
    // Alice cancels the very first order (100 left)
    val status1 = matcherCancelOrder(aliceNode, aliceWavesPair, aliceSell1)
    status1 should be("OrderCanceled")

    // Alice checks that the order book is empty
    val orders1 = matcherGetOrderBook()
    orders1.asks.size should be(0)
    orders1.bids.size should be(0)

    // Alice places a new sell order on 100
    val order = prepareOrder(aliceNode, aliceWavesPair, OrderType.SELL, 2 * Waves * Order.PriceConstant, 100)
    val (id, status2) = matcherPlaceOrder(order)
    status2 should be("OrderAccepted")

    // Alice checks that the order is in the order book
    val orders2 = matcherGetOrderBook()
    orders2.asks should contain(LevelResponse(20 * Waves / 10 * Order.PriceConstant, 100))
  }

  "buy order should execute all open orders and put remaining in order book" in {
    // Bob places buy order on amount bigger then left in sell orders
    val order = prepareOrder(bobNode, aliceWavesPair, OrderType.BUY, 2 * Waves * Order.PriceConstant, 130)
    val (id, status) = matcherPlaceOrder(order)
    status should be("OrderAccepted")

    // Check that the order is partially filled
    waitForOrderStatus(aliceAsset, id, "PartiallyFilled")

    // Check that remaining part of the order is in the order book
    val orders = matcherGetOrderBook()
    orders.bids should contain(LevelResponse(2 * Waves * Order.PriceConstant, 30))

    // Check balances
    waitForAssetBalance(aliceNode, aliceAsset, 850)
    waitForAssetBalance(bobNode, aliceAsset, 150)

    val updatedMatcherBalance = getBalance(matcherNode)
    updatedMatcherBalance._1 should be(matcherBalance._1 - TransactionFee + MatcherFee + (MatcherFee * 100.0 / 130.0).toLong)
    matcherBalance = updatedMatcherBalance

    val updatedBobBalance = getBalance(bobNode)
    updatedBobBalance._1 should be(bobBalance._1 - (MatcherFee * 100.0 / 130.0).toLong - 100 * 2 * Waves)
    bobBalance = updatedBobBalance

    val updatedAliceBalance = getBalance(aliceNode)
    updatedAliceBalance._1 should be(aliceBalance._1 - MatcherFee + 2 * Waves * 100)
    aliceBalance = updatedAliceBalance
  }

  "request order book for blacklisted pair" in {
    val f = matcherNode.matcherGetStatusCode(s"/matcher/orderbook/$ForbiddenAssetId/WAVES", 404)

    val result = Await.result(f, 1.minute)
    result.message shouldBe s"Invalid Asset ID: $ForbiddenAssetId"
  }

  "should consider UTX pool when checking the balance" in {
    // Bob issues new asset
    val bobAssetQuantity = 10000
    val bobAssetName = "BobCoin"
    val bobAsset = issueAsset(bobNode, bobAssetName, bobAssetQuantity)
    val bobAssetId = ByteStr.decodeBase58(bobAsset).get
    waitForAssetBalance(bobNode, bobAsset, bobAssetQuantity)
    waitForAssetBalance(matcherNode, bobAsset, 0)
    waitForAssetBalance(aliceNode, bobAsset, 0)

    // Bob wants to sell all own assets for 1 Wave
    val bobWavesPair = AssetPair(
      amountAsset = Some(bobAssetId),
      priceAsset = None
    )

    def bobOrder = prepareOrder(bobNode, bobWavesPair, OrderType.SELL, 1 * Waves * Order.PriceConstant, bobAssetQuantity)

    matcherPlaceOrder(bobOrder)

    // Alice wants to buy all Bob's assets for 1 Wave
    val (buyId, _) = matcherPlaceOrder(prepareOrder(aliceNode, bobWavesPair, OrderType.BUY, 1 * Waves * Order.PriceConstant, bobAssetQuantity))
    waitForOrderStatus(aliceAsset, buyId, "Filled")

    // Bob tries to do the same operation, but at now he have no assets
    matcherExpectOrderPlacementRejected(
      order = bobOrder,
      expectedStatusCode = 400,
      expectedStatus = "OrderRejected"
    )
  }

  private def issueAsset(node: Node, name: String, amount: Long): String = {
    val description = "asset for integration tests of matcher"
    val fee = 100000000L
    val futureIssueTransaction: Future[Transaction] = for {
      a <- node.issueAsset(node.address, name, description, amount, 0, fee, reissuable = false)
      _ <- node.waitForTransaction(a.id)
    } yield a

    val issueTransaction = Await.result(futureIssueTransaction, 1.minute)
    issueTransaction.id
  }

  private def waitForAssetBalance(node: Node, asset: String, expectedBalance: Long): Unit =
    Await.result(
      node.waitFor[AssetBalance](s"asset($asset) balance of ${node.address} >= $expectedBalance")(_.assetBalance(node.address, asset), _.balance >= expectedBalance, 5.seconds),
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

    val privateKey = node.privateKey
    val matcherPublicKey = matcherNode.publicKey

    Order(privateKey, matcherPublicKey, pair, orderType, price, amount, creationTime, timeToLive, MatcherFee)
  }

  private def matcherPlaceOrder(order: Order): (String, String) = {
    val futureResult = matcherNode.placeOrder(order)

    val result = Await.result(futureResult, 1.minute)

    (result.message.id, result.status)
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

  def waitForOrderStatus(asset: String, orderId: String, expectedStatus: String): Unit = Await.result(
    matcherNode.waitFor[MatcherStatusResponse](s"order(asset=$asset, orderId=$orderId) status == $expectedStatus")(_.getOrderStatus(asset, orderId), _.status == expectedStatus, 5.seconds),
    1.minute
  )

  def matcherGetOrderBook(): OrderBookResponse = {
    val futureResult = matcherNode.getOrderBook(aliceAsset)

    val result = Await.result(futureResult, 1.minute)

    result
  }

  private def matcherCancelOrder(node: Node, pair: AssetPair, orderId: String): String = {
    val privateKey = node.privateKey
    val publicKey = node.publicKey
    val request = CancelOrderRequest(publicKey, Base58.decode(orderId).get, Array.emptyByteArray)
    val signedRequest = CancelOrderRequest.sign(request, privateKey)
    val futureResult = matcherNode.cancelOrder(pair.amountAssetStr, pair.priceAssetStr, signedRequest)

    val result = Await.result(futureResult, 1.minute)

    result.status
  }

}

object MatcherTestSuite {

  import NodeConfigs.Default

  val ForbiddenAssetId = "FdbnAsset"

  private val matcherConfig = ConfigFactory.parseString(
    s"""
       |waves.matcher {
       |  enable=yes
       |  account="3Hm3LGoNPmw1VTZ3eRA2pAfeQPhnaBm6YFC"
       |  bind-address="0.0.0.0"
       |  order-match-tx-fee = 300000
       |  blacklisted-assets = [$ForbiddenAssetId]
       |}
       |waves.miner.enable=no
      """.stripMargin)

  private val nonGeneratingPeersConfig = ConfigFactory.parseString("waves.miner.enable=no")

  val AssetQuantity: Long = 1000

  val MatcherFee: Long = 300000
  val TransactionFee: Long = 300000

  val Waves: Long = 100000000L

  val Configs: Seq[Config] = Seq(matcherConfig.withFallback(Default.head)) ++
    Random.shuffle(Default.tail.init).take(2).map(nonGeneratingPeersConfig.withFallback(_)) ++
    Seq(Default.last)
}

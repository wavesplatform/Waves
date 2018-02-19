package com.wavesplatform.it
package matcher

import com.google.common.primitives.Longs
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.crypto
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.api._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.matcher.api.CancelOrderRequest
import com.wavesplatform.state2.ByteStr
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}
import play.api.libs.json.JsNumber
import play.api.libs.json.Json.parse
import scorex.api.http.assets.SignedTransferRequest
import scorex.crypto.encode.Base58
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Random

class MatcherTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with NodesFromDocker
  with ReportingTestName with MatcherUtils {

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

    scorex.account.AddressScheme.current = new scorex.account.AddressScheme {
      override val chainId: Byte = 'I'
    }

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
      .getResponseBody.stripPrefix("\"").stripSuffix("\"") shouldBe matcherNode.publicKeyStr
  }

  "sell order could be placed" in {
    // Alice places sell order
    val (id, status) = matcherPlaceOrder(matcherNode,
      prepareOrder(aliceNode, matcherNode, aliceWavesPair, OrderType.SELL, 2 * Waves * Order.PriceConstant, aliceSellAmount))
    status shouldBe "OrderAccepted"
    aliceSell1 = id
    // Alice checks that the order in order book
    matcherCheckOrderStatus(matcherNode, aliceAsset, id) shouldBe "Accepted"

    // Alice check that order is correct
    val orders = matcherGetOrderBook(matcherNode, aliceAsset)
    orders.asks.head.amount shouldBe aliceSellAmount
    orders.asks.head.price shouldBe 2 * Waves * Order.PriceConstant
  }

  "frozen amount should be listed via matcherBalance REST endpoint" in {
    val ts = System.currentTimeMillis()
    val privateKey = aliceNode.privateKey
    val signature = Base58.encode(crypto.sign(privateKey, aliceNode.publicKey.publicKey ++ Longs.toByteArray(ts)))

    val json = parse(Await.result(matcherNode.matcherGet(s"/matcher/matcherBalance/${aliceNode.publicKeyStr}", _
      .addHeader("Timestamp", ts)
      .addHeader("Signature", signature)), 1.minute).getResponseBody)

    (json \ aliceAsset).get shouldBe JsNumber(aliceSellAmount)
  }

  "and should be listed by trader's publiс key via REST" in {
    val ts = System.currentTimeMillis()
    val privateKey = aliceNode.privateKey
    val signature = ByteStr(crypto.sign(privateKey, aliceNode.publicKey.publicKey ++ Longs.toByteArray(ts)))
    val orderIds = Await.result(matcherNode.getOrderbookByPublicKey(aliceNode.publicKeyStr, ts, signature), 1.minute)
      .map(_.id)

    orderIds should contain(aliceSell1)
  }

  "and should match with buy order" in {
    // Bob places a buy order
    val (id, status) = matcherPlaceOrder(matcherNode,
      prepareOrder(bobNode, matcherNode, aliceWavesPair, OrderType.BUY, 2 * Waves * Order.PriceConstant, 200))
    bobBuy1 = id
    status shouldBe "OrderAccepted"

    waitForOrderStatus(matcherNode, aliceAsset, aliceSell1, "PartiallyFilled")

    // Bob waits for order to fill
    waitForOrderStatus(matcherNode, aliceAsset, bobBuy1, "Filled")

    // Bob checks that asset on his balance
    waitForAssetBalance(bobNode, aliceAsset, 200)

    // Alice checks that part of her order still in the order book
    val orders = matcherGetOrderBook(matcherNode, aliceAsset)
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
    val badOrder = prepareOrder(bobNode, matcherNode, aliceWavesPair, OrderType.SELL, (19.0 * Waves / 10.0 * Order.PriceConstant).toLong, 300)
    val error = matcherExpectOrderPlacementRejected(badOrder, 400, "OrderRejected")
    error should be(true)

    // Bob places order on available amount of assets - order accepted
    val goodOrder = prepareOrder(bobNode, matcherNode, aliceWavesPair, OrderType.SELL, (19.0 * Waves / 10.0 * Order.PriceConstant).toLong, 150)
    val (_, status) = matcherPlaceOrder(matcherNode, goodOrder)
    status should be("OrderAccepted")

    // Bob checks that the order in the order book
    val orders = matcherGetOrderBook(matcherNode, aliceAsset)
    orders.asks should contain(LevelResponse(19 * Waves / 10 * Order.PriceConstant, 150))
  }

  "buy order should match on few price levels" in {
    // Alice places a buy order
    val order = prepareOrder(aliceNode, matcherNode, aliceWavesPair, OrderType.BUY, (21.0 * Waves / 10.0 * Order.PriceConstant).toLong, 350)
    val (id, status) = matcherPlaceOrder(matcherNode, order)
    status should be("OrderAccepted")

    // Where were 2 sells that should fulfill placed order
    waitForOrderStatus(matcherNode, aliceAsset, id, "Filled")

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
    val orders1 = matcherGetOrderBook(matcherNode, aliceAsset)
    orders1.asks.size should be(0)
    orders1.bids.size should be(0)

    // Alice places a new sell order on 100
    val order = prepareOrder(aliceNode, matcherNode, aliceWavesPair, OrderType.SELL, 2 * Waves * Order.PriceConstant, 100)
    val (id, status2) = matcherPlaceOrder(matcherNode, order)
    status2 should be("OrderAccepted")

    // Alice checks that the order is in the order book
    val orders2 = matcherGetOrderBook(matcherNode, aliceAsset)
    orders2.asks should contain(LevelResponse(20 * Waves / 10 * Order.PriceConstant, 100))
  }

  "buy order should execute all open orders and put remaining in order book" in {
    // Bob places buy order on amount bigger then left in sell orders
    val order = prepareOrder(bobNode, matcherNode, aliceWavesPair, OrderType.BUY, 2 * Waves * Order.PriceConstant, 130)
    val (id, status) = matcherPlaceOrder(matcherNode, order)
    status should be("OrderAccepted")

    // Check that the order is partially filled
    waitForOrderStatus(matcherNode, aliceAsset, id, "PartiallyFilled")

    // Check that remaining part of the order is in the order book
    val orders = matcherGetOrderBook(matcherNode, aliceAsset)
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

    Await.result(matcherNode.waitForHeightArise, 1.minute)

    def bobOrder = prepareOrder(bobNode, matcherNode, bobWavesPair, OrderType.SELL, 1 * Waves * Order.PriceConstant, bobAssetQuantity)

    val (sellId, _) = matcherPlaceOrder(matcherNode, bobOrder)
    waitForOrderStatus(matcherNode, aliceAsset, sellId, "Accepted")

    // Alice wants to buy all Bob's assets for 1 Wave
    val (buyId, _) = matcherPlaceOrder(matcherNode, prepareOrder(aliceNode, matcherNode, bobWavesPair, OrderType.BUY, 1 * Waves * Order.PriceConstant, bobAssetQuantity))
    waitForOrderStatus(matcherNode, aliceAsset, buyId, "Filled")

    // Bob tries to do the same operation, but at now he have no assets
    matcherExpectOrderPlacementRejected(
      order = bobOrder,
      expectedStatusCode = 400,
      expectedStatus = "OrderRejected"
    )
  }

  "should cancel a sell order if the owner moved its assets to another account" in {
    // Bob issues new asset
    val bobAssetQuantity = 10000
    val bobAssetName = "NewBobCoin"
    val bobAsset = issueAsset(bobNode, bobAssetName, bobAssetQuantity)
    val bobAssetId = ByteStr.decodeBase58(bobAsset).get
    waitForAssetBalance(bobNode, bobAsset, bobAssetQuantity)

    // Bob wants to sell all own assets for 1 Wave
    val bobWavesPair = AssetPair(
      amountAsset = Some(bobAssetId),
      priceAsset = None
    )

    Await.result(matcherNode.waitForHeightArise, 1.minute)

    def bobOrder = prepareOrder(bobNode, matcherNode, bobWavesPair, OrderType.SELL, 1 * Waves * Order.PriceConstant, bobAssetQuantity)

    val (sellId, _) = matcherPlaceOrder(matcherNode, bobOrder)
    waitForOrderStatus(matcherNode, bobAsset, sellId, "Accepted")

    // Bob moves all his tokens to Alice
    val transferTx = TransferTransaction.create(
      assetId = Some(bobAssetId),
      sender = bobNode.privateKey,
      recipient = scorex.account.Address.fromBytes(Base58.decode(aliceNode.address).get).right.get,
      amount = bobAssetQuantity,
      timestamp = System.currentTimeMillis(),
      feeAssetId = None,
      feeAmount = 100000,
      attachment = Array.emptyByteArray
    ).right.get

    Await.result(matcherNode.signedTransfer(createSignedTransferRequest(transferTx)), 1.minute)

    val waitOrderCancelled = matcherNode.waitFor[MatcherStatusResponse]("Order is cancelled")(_.getOrderStatus(bobAssetName, sellId), _.status == "Cancelled", 1.second)
    Await.result(waitOrderCancelled, 1.minute)
  }

  private def matcherExpectOrderPlacementRejected(order: Order, expectedStatusCode: Int, expectedStatus: String): Boolean = {
    val futureResult = matcherNode.expectIncorrectOrderPlacement(order, expectedStatusCode, expectedStatus)

    Await.result(futureResult, 1.minute)
  }

  private def matcherCancelOrder(node: Node, pair: AssetPair, orderId: String): String = {
    val privateKey = node.privateKey
    val publicKey = node.publicKey
    val request = CancelOrderRequest(publicKey, Base58.decode(orderId).get, Array.emptyByteArray)
    val sig = crypto.sign(privateKey, request.toSign)
    val signedRequest = request.copy(signature = sig)
    val futureResult = matcherNode.cancelOrder(pair.amountAssetStr, pair.priceAssetStr, signedRequest)

    val result = Await.result(futureResult, 1.minute)

    result.status
  }

  protected def createSignedTransferRequest(tx: TransferTransaction): SignedTransferRequest = {
    import tx._
    SignedTransferRequest(
      Base58.encode(tx.sender.publicKey),
      assetId.map(_.base58),
      recipient.stringRepr,
      amount,
      fee,
      feeAssetId.map(_.base58),
      timestamp,
      attachment.headOption.map(_ => Base58.encode(attachment)),
      signature.base58
    )
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
       |  blacklisted-assets = ["$ForbiddenAssetId"]
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

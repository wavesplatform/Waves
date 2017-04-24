package com.wavesplatform.matcher.integration

import com.wavesplatform.matcher.api.CancelOrderRequest
import com.wavesplatform.settings.Constants
import org.scalatest.concurrent.Eventually
import org.scalatest.time.SpanSugar._
import org.scalatest.{DoNotDiscover, FunSuite, Matchers}
import play.api.libs.json.{JsArray, JsValue, Json}
import scorex.account.{Account, PrivateKeyAccount}
import scorex.api.http.assets.{IssueRequest, TransferRequest}
import scorex.crypto.encode.Base58
import scorex.transaction.AssetAcc
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}
import scorex.transaction.assets.exchange.OrderJson._
import scorex.utils.NTP

/**
  * !!! Tests should work only as whole TestSuite in sequence one by one, not separately,
  * as the state depends on the previous test
  */
class MatcherAPISpecification extends FunSuite with Matchers with Eventually with scorex.waves.TestingCommons {
  private val wallet = application.wallet
  private val AccountM = wallet.privateKeyAccounts()(2)
  private val AccountA = wallet.privateKeyAccounts().head
  private var Asset1 = Option("")
  private var TradingPair = AssetPair(None, None)
  private var MBalance = 0L
  private var MBalance1 = 0L
  private var ABalance = 0L
  private var ABalance1 = 0L
  private val TxFee = 100000L
  private implicit val storedState = application.storedState
  private var orderIdToCancel = Option.empty[String]

  private val MatcherPubKey = application.wallet.findWallet(application.settings.matcherSettings.account).
    map(a => Base58.encode(a.publicKey)).right.get

  def initBalances() = {
    assetTransfer(AccountM, AccountA, 5000 * Constants.UnitsInWave)
    Asset1 = Some(issueAsset(AccountM, 1000 * Constants.UnitsInWave))
    TradingPair = AssetPair(Base58.decode(Asset1.get).toOption, None)
    MBalance = storedState.assetBalance(AssetAcc(AccountM, None))
    MBalance1 = storedState.assetBalance(AssetAcc(AccountM, Asset1.flatMap(Base58.decode(_).toOption)))
    ABalance = storedState.assetBalance(AssetAcc(AccountA, None))
    ABalance1 = storedState.assetBalance(AssetAcc(AccountA, Asset1.flatMap(Base58.decode(_).toOption)))
  }

  def issueAsset(from: Account, amount: Long): String = {
    val json =
      s"""{
         |  "name": "string",
         |  "quantity": $amount,
         |  "description": "string",
         |  "sender": "${from.address}",
         |  "decimals": 8,
         |  "reissuable": true,
         |  "fee": 100000000
         |}""".stripMargin
    val req = Json.parse(json).validate[IssueRequest].get
    val resp = POST.requestJson(us = "/assets/issue", body = json)
    val id = (resp \ "id").as[String]
    id should not be empty

    waitForBalance(amount, from, Some(id))
    id
  }

  def placeBuy(acc: PrivateKeyAccount,
               price: Double, amount: Long, expectedStatus: String = "OrderAccepted"): Option[String] = {
    placeOrder(acc, TradingPair, OrderType.BUY, price, amount, expectedStatus)
  }

  def placeSell(acc: PrivateKeyAccount,
               price: Double, amount: Long, expectedStatus: String = "OrderAccepted"): Option[String] = {
    placeOrder(acc, TradingPair, OrderType.SELL, price, amount, expectedStatus)
  }


  def placeOrder(acc: PrivateKeyAccount, assetPair: AssetPair, orderType: OrderType,
                 price: Double, amount: Long, expectedStatus: String = "OrderAccepted"): Option[String] = {
    val created = NTP.correctedTime()
    val timeToLive = created + Order.MaxLiveTime - 1000
    val pubKeyStr = Base58.encode(acc.publicKey)
    val json =
      s"""{
         |  "matcherFee": 100000,
         |  "price": ${(price * Order.PriceConstant).toLong},
         |  "assetPair": ${Json.stringify(assetPair.json)},
         |  "orderType": "$orderType",
         |  "amount": $amount,
         |  "timestamp": $created,
         |  "expiration": $timeToLive,
         |  "matcherPublicKey": "$MatcherPubKey",
         |  "senderPublicKey": "$pubKeyStr"
         |}""".stripMargin
    val order = Json.parse(json).validate[Order].get
    val signed = Order.sign(order, acc)
    val signedJson = signed.json

    val resp = POST.requestJson("/orderbook", body = signedJson.toString, peer = matcherUrl())

    (resp \ "status").as[String] shouldBe expectedStatus
    (resp \ "message" \ "id").toOption.map(_.as[String])
  }

  def getOrderBook(asset: Option[String]): JsValue = {
    GET.requestJson(s"/orderbook/${asset.get}/WAVES", peer = matcherUrl())
  }

  def getOrderStatus(asset: Option[String], id: String): JsValue = {
    GET.requestJson(s"/orderbook/${asset.get}/WAVES/$id", peer = matcherUrl())
  }

  def waitForOrderStatus(asset: Option[String], id: String, status: String) = {
    eventually(timeout(5.seconds), interval(500.millis)) {
      (getOrderStatus(Asset1, id) \ "status").as[String] should be("Filled")
    }
  }

  def cancelOrder(acc: PrivateKeyAccount,
                  orderId: String, expectedStatus: String = "OrderCanceled"): Unit = {
    val ts = NTP.correctedTime()
    val pubKeyStr = Base58.encode(acc.publicKey)
    val json =
      s"""{
         |  "sender": "$pubKeyStr",
         |  "orderId": "$orderId",
         |  "signature": "signature"
         |}""".stripMargin
    val orderCancel = Json.parse(json).validate[CancelOrderRequest].get
    val signed = CancelOrderRequest.sign(orderCancel, acc)
    val signedJson = signed.json

    val resp = POST.requestJson(s"/orderbook/${TradingPair.amountAssetStr}/${TradingPair.priceAssetStr}/cancel", body = signedJson.toString, peer = matcherUrl())

    (resp \ "status").as[String] shouldBe expectedStatus
  }

  test("start") {
    // don't move this to `beforeAll`! if this fails, `afterAll` never happens, leading to ports remain open
    waitForSingleConnection(application)
    waitForNextBlock(application)
    initBalances()
    Thread.sleep(1000)
  }

  test("/matcher/") {
    val resp = GET.requestJson("/", peer = matcherUrl())
    resp.as[String] shouldBe MatcherPubKey
  }

  test("place sell order") {
    orderIdToCancel = placeSell(AccountM, 2, 500 * Constants.UnitsInWave)
    val ob = getOrderBook(Asset1)
    ((ob \ "asks") (0) \ "price").as[Long] shouldBe 2 * Order.PriceConstant
    ((ob \ "asks") (0) \ "amount").as[Long] shouldBe 500 * Constants.UnitsInWave
  }

  test("match with buy order") {
    val id = placeBuy(AccountA, 2, 200 * Constants.UnitsInWave)
    val ob = getOrderBook(Asset1)
    ((ob \ "asks") (0) \ "amount").as[Long] shouldBe 300 * Constants.UnitsInWave

    val executedFee = 100000L
    MBalance += 2 * 200 * Constants.UnitsInWave + executedFee
    waitForBalance(MBalance, AccountM, None)
    ABalance -= 2 * 200 * Constants.UnitsInWave + executedFee
    waitForBalance(ABalance, AccountA, None)

    MBalance1 -= 200 * Constants.UnitsInWave
    waitForBalance(MBalance1, AccountM, Asset1)
    ABalance1 += 200 * Constants.UnitsInWave
    waitForBalance(ABalance1, AccountA, Asset1)

    (getOrderStatus(Asset1, id.get) \ "status").as[String] should be("Filled")
  }

  test("submit more orders than available assets including open") {
    waitForBalance(800 * Constants.UnitsInWave, AccountM, Asset1) // And 300 by price = 2 is open
    // Should be available Asset1 = 800 - 300 = 500 Asset1
    placeSell(AccountM, 1.5, 501 * Constants.UnitsInWave, "OrderRejected")
    placeSell(AccountM, 1.5, 500 * Constants.UnitsInWave, "OrderAccepted")

    val ob = getOrderBook(Asset1)
    ((ob \ "asks") (0) \ "price").as[Long] shouldBe (1.5 * Order.PriceConstant).toLong
    ((ob \ "asks") (0) \ "amount").as[Long] shouldBe 500 * Constants.UnitsInWave
    ((ob \ "asks") (1) \ "price").as[Long] shouldBe (2 * Order.PriceConstant).toLong
    ((ob \ "asks") (1) \ "amount").as[Long] shouldBe 300 * Constants.UnitsInWave
  }

  test("buy order match several price levels") {
    val id = placeBuy(AccountA, 2.5, 700 * Constants.UnitsInWave, "OrderAccepted")
    waitForOrderStatus(Asset1, id.get, "Filled")

    val wavesAmount = (1.5 * 500 + 2 * 200).toLong * Constants.UnitsInWave
    val executedFee =  100000L * 500 / 700 + 100000L * 200 / 700
    MBalance += wavesAmount + executedFee
    waitForBalance(MBalance, AccountM, None)
    ABalance -= wavesAmount + executedFee
    waitForBalance(ABalance, AccountA, None)

    val ob = getOrderBook(Asset1)
    ((ob \ "asks") (0) \ "price").as[Long] shouldBe (2 * Order.PriceConstant).toLong
    ((ob \ "asks") (0) \ "amount").as[Long] shouldBe 100 * Constants.UnitsInWave

    val assetAmount = (500 + 200) * Constants.UnitsInWave
    MBalance1 -= assetAmount // shouldBe 100
    waitForBalance(MBalance1, AccountM, Asset1)
    ABalance1 += assetAmount // shouldBe 900
    waitForBalance(ABalance1, AccountA, Asset1)
  }

  test("cancel order and resubmit a new one") {
    cancelOrder(AccountM, orderIdToCancel.get)
    placeSell(AccountM, 5, 100 * Constants.UnitsInWave, "OrderAccepted")
  }

  test("buy order should execute all open orders and put remaining in OrderBook") {
    waitForBalance(ABalance, AccountA, None)
    placeBuy(AccountA, 5.5, 250 * Constants.UnitsInWave, "OrderAccepted")
    MBalance1 = 0
    waitForBalance(MBalance1, AccountM, Asset1)
    ABalance1 = 1000 * Constants.UnitsInWave
    waitForBalance(ABalance1, AccountA, Asset1)
    ABalance -= 500 * Constants.UnitsInWave + (TxFee * 100 / 250)
    waitForBalance(ABalance, AccountA, None)

    val ob = getOrderBook(Asset1)
    ((ob \ "bids") (0) \ "price").as[Long] shouldBe (5.5 * Order.PriceConstant).toLong
    ((ob \ "bids") (0) \ "amount").as[Long] shouldBe 150 * Constants.UnitsInWave
    (ob \ "asks").get.asInstanceOf[JsArray].value.size shouldBe 0
  }
}

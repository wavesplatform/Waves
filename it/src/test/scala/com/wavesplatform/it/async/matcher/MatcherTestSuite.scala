package com.wavesplatform.it.async.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.crypto
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.api.{LevelResponse, MatcherStatusResponse}
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{Node, ReportingTestName}
import com.wavesplatform.matcher.api.CancelOrderRequest
import com.wavesplatform.matcher.market.MatcherActor
import com.wavesplatform.state.{ByteStr, EitherExt2}
import com.wavesplatform.utils.Base58
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}
import scorex.api.http.assets.SignedTransferV1Request
import scorex.api.http.leasing.{SignedLeaseCancelV1Request, SignedLeaseV1Request}
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}
import scorex.transaction.lease.{LeaseCancelTransactionV1, LeaseTransactionV1}
import scorex.transaction.transfer._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Random

class MatcherTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with NodesFromDocker
    with ReportingTestName
    with MatcherUtils {

  import MatcherTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  private var matcherBalance = (0L, 0L)
  private var aliceBalance   = (0L, 0L)
  private var bobBalance     = (0L, 0L)

  private val aliceSellAmount = 500
  private var aliceSell1      = ""
  private var bobBuy1         = ""

  private var aliceAsset     = ""
  private var aliceAssetId   = ByteStr.empty
  private var aliceWavesPair = AssetPair(None, None)

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    // Store initial balances of participants
    matcherBalance = getBalance(matcherNode)
    bobBalance = getBalance(bobNode)

    // Alice issues new asset
    aliceAsset = issueAsset(aliceNode, "AliceCoin", AssetQuantity)
    aliceAssetId = ByteStr.decodeBase58(aliceAsset).get
    aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    // Wait for balance on Alice's account
    Await.result(matcherNode.waitForHeightArise, 1.minute)
    waitForAssetBalance(aliceNode, aliceAsset, AssetQuantity)
    waitForAssetBalance(matcherNode, aliceAsset, 0)
    waitForAssetBalance(bobNode, aliceAsset, 0)

    // Alice spent 1 Wave to issue the asset
    aliceBalance = getBalance(aliceNode)
  }

  "matcher should respond with Public key" in {
    Await.result(matcherNode.matcherGet("/matcher"), 1.minute).getResponseBody.stripPrefix("\"").stripSuffix("\"") shouldBe matcherNode.publicKeyStr
  }

  "sell order could be placed" in {
    // Alice places sell order
    val (id, status) =
      matcherPlaceOrder(matcherNode,
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
    getReservedBalance(matcherNode, aliceNode.privateKey) shouldBe Map(aliceAsset -> aliceSellAmount)

    getReservedBalance(matcherNode, bobNode.privateKey) shouldBe Map()
  }

  "and should be listed by trader's publiс key via REST" in {

    val orderIds = getAllOrder(matcherNode, aliceNode.privateKey)

    orderIds should contain(aliceSell1)
  }

  "and should match with buy order" in {
    // Bob places a buy order
    val (id, status) =
      matcherPlaceOrder(matcherNode, prepareOrder(bobNode, matcherNode, aliceWavesPair, OrderType.BUY, 2 * Waves * Order.PriceConstant, 200))
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

  "request activeOnly orders" in {
    val aliceOrders = getAllActiveOrder(matcherNode, aliceNode.privateKey)
    aliceOrders shouldBe Seq(aliceSell1)
    val bobOrders = getAllActiveOrder(matcherNode, bobNode.privateKey)
    bobOrders shouldBe Seq()
  }

  "submitting sell orders should check availability of asset" in {
    // Bob trying to place order on more assets than he has - order rejected
    val badOrder = prepareOrder(bobNode, matcherNode, aliceWavesPair, OrderType.SELL, (19.0 * Waves / 10.0 * Order.PriceConstant).toLong, 300)
    val error    = matcherExpectOrderPlacementRejected(badOrder, 400, "OrderRejected")
    error should be(true)

    // Bob places order on available amount of assets - order accepted
    val goodOrder   = prepareOrder(bobNode, matcherNode, aliceWavesPair, OrderType.SELL, (19.0 * Waves / 10.0 * Order.PriceConstant).toLong, 150)
    val (_, status) = matcherPlaceOrder(matcherNode, goodOrder)
    status should be("OrderAccepted")

    // Bob checks that the order in the order book
    val orders = matcherGetOrderBook(matcherNode, aliceAsset)
    orders.asks should contain(LevelResponse(19 * Waves / 10 * Order.PriceConstant, 150))
  }

  "buy order should match on few price levels" in {
    // Alice places a buy order
    val order        = prepareOrder(aliceNode, matcherNode, aliceWavesPair, OrderType.BUY, (21.0 * Waves / 10.0 * Order.PriceConstant).toLong, 350)
    val (id, status) = matcherPlaceOrder(matcherNode, order)
    status should be("OrderAccepted")

    // Where were 2 sells that should fulfill placed order
    waitForOrderStatus(matcherNode, aliceAsset, id, "Filled")

    // Check balances
    waitForAssetBalance(aliceNode, aliceAsset, 950)
    waitForAssetBalance(bobNode, aliceAsset, 50)

    val updatedMatcherBalance = getBalance(matcherNode)
    updatedMatcherBalance._1 should be(
      matcherBalance._1 - 2 * TransactionFee + MatcherFee + (MatcherFee * 150.0 / 350.0).toLong + (MatcherFee * 200.0 / 350.0).toLong + (MatcherFee * 200.0 / 500.0).toLong)
    matcherBalance = updatedMatcherBalance

    val updatedBobBalance = getBalance(bobNode)
    updatedBobBalance._1 should be(bobBalance._1 - MatcherFee + 150 * (19.0 * Waves / 10.0).toLong)
    bobBalance = updatedBobBalance

    val updatedAliceBalance = getBalance(aliceNode)
    updatedAliceBalance._1 should be(
      aliceBalance._1 - (MatcherFee * 200.0 / 350.0).toLong - (MatcherFee * 150.0 / 350.0).toLong - (MatcherFee * 200.0 / 500.0).toLong - (19.0 * Waves / 10.0).toLong * 150)
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
    val order         = prepareOrder(aliceNode, matcherNode, aliceWavesPair, OrderType.SELL, 2 * Waves * Order.PriceConstant, 100)
    val (id, status2) = matcherPlaceOrder(matcherNode, order)
    status2 should be("OrderAccepted")

    // Alice checks that the order is in the order book
    val orders2 = matcherGetOrderBook(matcherNode, aliceAsset)
    orders2.asks should contain(LevelResponse(20 * Waves / 10 * Order.PriceConstant, 100))
  }

  "buy order should execute all open orders and put remaining in order book" in {
    // Bob places buy order on amount bigger then left in sell orders
    val order        = prepareOrder(bobNode, matcherNode, aliceWavesPair, OrderType.BUY, 2 * Waves * Order.PriceConstant, 130)
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
    val bobAssetName     = "BobCoin"
    val bobAsset         = issueAsset(bobNode, bobAssetName, bobAssetQuantity)
    val bobAssetId       = ByteStr.decodeBase58(bobAsset).get

    Await.result(matcherNode.waitForHeightArise, 1.minute)
    waitForAssetBalance(bobNode, bobAsset, bobAssetQuantity)
    waitForAssetBalance(matcherNode, bobAsset, 0)
    waitForAssetBalance(aliceNode, bobAsset, 0)

    // Bob wants to sell all own assets for 1 Wave
    val bobWavesPair = AssetPair(
      amountAsset = Some(bobAssetId),
      priceAsset = None
    )

    def bobOrder = prepareOrder(bobNode, matcherNode, bobWavesPair, OrderType.SELL, 1 * Waves * Order.PriceConstant, bobAssetQuantity)

    val (sellId, _) = matcherPlaceOrder(matcherNode, bobOrder)
    waitForOrderStatus(matcherNode, aliceAsset, sellId, "Accepted")

    // Alice wants to buy all Bob's assets for 1 Wave
    val (buyId, _) =
      matcherPlaceOrder(matcherNode,
                        prepareOrder(aliceNode, matcherNode, bobWavesPair, OrderType.BUY, 1 * Waves * Order.PriceConstant, bobAssetQuantity))
    waitForOrderStatus(matcherNode, aliceAsset, buyId, "Filled")

    // Bob tries to do the same operation, but at now he have no assets
    matcherExpectOrderPlacementRejected(
      order = bobOrder,
      expectedStatusCode = 400,
      expectedStatus = "OrderRejected"
    )
  }

  "trader should be able to place a buy waves for asset order without having waves" in {
    // Bob issues new asset
    val bobAssetQuantity = 10000
    val bobAssetName     = "BobCoin2"
    val bobAsset         = issueAsset(bobNode, bobAssetName, bobAssetQuantity)
    val bobAssetId       = ByteStr.decodeBase58(bobAsset).get
    val bobWavesPair = AssetPair(
      amountAsset = Some(bobAssetId),
      priceAsset = None
    )

    Await.result(matcherNode.waitForHeightArise, 1.minute)
    waitForAssetBalance(bobNode, bobAsset, bobAssetQuantity)
    waitForAssetBalance(matcherNode, bobAsset, 0)
    waitForAssetBalance(aliceNode, bobAsset, 0)

    // Bob wants to sell all own assets for 1 Wave
    def bobOrder = prepareOrder(bobNode, matcherNode, bobWavesPair, OrderType.SELL, 1 * Waves * Order.PriceConstant, bobAssetQuantity)

    val (sellId, _) = matcherPlaceOrder(matcherNode, bobOrder)
    waitForOrderStatus(matcherNode, bobAsset, sellId, "Accepted")

    // Bob moves all waves to Alice
    val bobBalance     = Await.result(matcherNode.balance(bobNode.address), 1.minute).balance
    val transferAmount = bobBalance - TransactionFee
    transfer(bobNode, aliceNode, None, transferAmount, wait = true)

    val newBobBalance = Await.result(matcherNode.balance(bobNode.address), 1.minute).balance
    newBobBalance shouldBe 0

    // Order should stay accepted
    checkOrderStatusDontChange(matcherNode, bobAsset, sellId, "Accepted")

    // Cleanup
    Await.ready(matcherNode.waitForHeightArise, 1.minute)
    matcherCancelOrder(bobNode, bobWavesPair, sellId) should be("OrderCanceled")
    transfer(aliceNode, bobNode, None, transferAmount, wait = true)
    Await.ready(matcherNode.waitForHeightArise, 1.minute)
  }

  "owner moves assets/waves to another account and order become an invalid" - {
    val bobAssetName             = "BobCoin3"
    var bobAssetIdRaw: String    = ""
    var bobAssetId: ByteStr      = ByteStr.empty
    var bobWavesPair: AssetPair  = AssetPair(None, None)
    var twoAssetsPair: AssetPair = AssetPair(None, None)

    "prepare" in {
      // Bob issues a new asset
      val bobAssetQuantity = 10000
      bobAssetIdRaw = issueAsset(bobNode, bobAssetName, bobAssetQuantity)
      bobAssetId = ByteStr.decodeBase58(bobAssetIdRaw).get
      bobWavesPair = AssetPair(
        amountAsset = Some(bobAssetId),
        priceAsset = None
      )

      twoAssetsPair =
        if (MatcherActor.compare(Some(bobAssetId.arr), Some(aliceAssetId.arr)) < 0)
          AssetPair(
            amountAsset = Some(aliceAssetId),
            priceAsset = Some(bobAssetId)
          )
        else
          AssetPair(
            amountAsset = Some(bobAssetId),
            priceAsset = Some(aliceAssetId)
          )

      Await.result(matcherNode.waitForHeightArise, 1.minute)
      waitForAssetBalance(bobNode, bobAssetIdRaw, bobAssetQuantity)
    }

    // Could not work sometimes because of NODE-546
    "order with assets" ignore {
      "moved assets, insufficient assets" in {
        val oldestOrderId = bobPlacesAssetOrder(8000)
        val newestOrderId = bobPlacesAssetOrder(1000)

        transfer(bobNode, aliceNode, Some(bobAssetId), 3050)

        withClue(s"The oldest order '$oldestOrderId' was cancelled") {
          waitOrderCancelled(oldestOrderId)
        }
        withClue(s"The newest order '$newestOrderId' is still active") {
          checkBobOrderActive(newestOrderId)
        }

        // Cleanup
        Await.ready(matcherNode.waitForHeightArise, 1.minute)
        matcherCancelOrder(bobNode, twoAssetsPair, newestOrderId) should be("OrderCanceled")
        transfer(aliceNode, bobNode, Some(bobAssetId), 3050, wait = true)
        Await.ready(matcherNode.waitForHeightArise, 1.minute)
      }

      "leased waves, insufficient fee" in {
        val bobBalance    = Await.result(matcherNode.balance(bobNode.address), 1.minute).balance
        val oldestOrderId = bobPlacesAssetOrder(1000)
        val newestOrderId = bobPlacesAssetOrder(1000)

        // TransactionFee for leasing, MatcherFee for one order
        val leaseAmount = bobBalance - TransactionFee - MatcherFee
        val leaseId     = lease(bobNode, aliceNode, leaseAmount)

        withClue(s"The oldest order '$oldestOrderId' was cancelled") {
          waitOrderCancelled(oldestOrderId)
        }
        withClue(s"The newest order '$newestOrderId' is still active") {
          checkBobOrderActive(newestOrderId)
        }

        // Cleanup
        Await.ready(matcherNode.waitForHeightArise, 1.minute)
        matcherCancelOrder(bobNode, twoAssetsPair, newestOrderId) should be("OrderCanceled")
        cancelLease(bobNode, leaseId, leaseAmount)
        Await.ready(matcherNode.waitForHeightArise, 1.minute)
      }

      "moved waves, insufficient fee" in {
        val bobBalance    = Await.result(matcherNode.balance(bobNode.address), 1.minute).balance
        val oldestOrderId = bobPlacesAssetOrder(1000)
        val newestOrderId = bobPlacesAssetOrder(1000)

        // TransactionFee for leasing, MatcherFee for one order
        val transferAmount = bobBalance - TransactionFee - MatcherFee
        transfer(bobNode, aliceNode, None, transferAmount)

        withClue(s"The oldest order '$oldestOrderId' was cancelled") {
          waitOrderCancelled(oldestOrderId)
        }
        withClue(s"The newest order '$newestOrderId' is still active") {
          checkBobOrderActive(newestOrderId)
        }

        // Cleanup
        Await.ready(matcherNode.waitForHeightArise, 1.minute)
        matcherCancelOrder(bobNode, twoAssetsPair, newestOrderId) should be("OrderCanceled")
        transfer(aliceNode, bobNode, None, transferAmount, wait = true)
        Await.ready(matcherNode.waitForHeightArise, 1.minute)
      }
    }

    "order with waves" - {
      "leased waves, insufficient fee" in {
        // Amount of waves in order is smaller than fee
        val bobBalance = Await.result(matcherNode.balance(bobNode.address), 1.minute).balance

        val price   = TransactionFee / 2
        val orderId = bobPlacesWavesOrder(price * Order.PriceConstant, 1)

        val leaseAmount = bobBalance - TransactionFee - price
        val leaseId     = lease(bobNode, aliceNode, leaseAmount)

        withClue(s"The order '$orderId' was cancelled") {
          waitOrderCancelled(orderId)
        }

        // Cleanup
        Await.ready(matcherNode.waitForHeightArise, 1.minute)
        cancelLease(bobNode, leaseId, leaseAmount)
        Await.ready(matcherNode.waitForHeightArise, 1.minute)
      }

      "leased waves, insufficient waves" in {
        val bobBalance = Await.result(matcherNode.balance(bobNode.address), 1.minute).balance

        val price   = 1 * Waves
        val orderId = bobPlacesWavesOrder(price * Order.PriceConstant, 1)

        val leaseAmount = bobBalance - TransactionFee - price / 2
        val leaseId     = lease(bobNode, aliceNode, leaseAmount)

        withClue(s"The order '$orderId' was cancelled") {
          waitOrderCancelled(orderId)
        }

        // Cleanup
        Await.ready(matcherNode.waitForHeightArise, 1.minute)
        cancelLease(bobNode, leaseId, leaseAmount)
        Await.ready(matcherNode.waitForHeightArise, 1.minute)
      }

      "moved waves, insufficient fee" in {
        // Amount of waves in order is smaller than fee
        val bobBalance = Await.result(matcherNode.balance(bobNode.address), 1.minute).balance

        val price   = TransactionFee / 2
        val orderId = bobPlacesWavesOrder(price * Order.PriceConstant, 1)

        val transferAmount = bobBalance - TransactionFee - price
        transfer(bobNode, aliceNode, None, transferAmount)

        withClue(s"The order '$orderId' was cancelled") {
          waitOrderCancelled(orderId)
        }

        // Cleanup
        Await.ready(matcherNode.waitForHeightArise, 1.minute)
        transfer(aliceNode, bobNode, None, transferAmount)
        Await.ready(matcherNode.waitForHeightArise, 1.minute)
      }
    }

    def bobPlacesWavesOrder(price: Long, amount: Int): String = {
      val bobOrder = prepareOrder(bobNode, matcherNode, bobWavesPair, OrderType.BUY, price, amount)
      val (id, _)  = matcherPlaceOrder(matcherNode, bobOrder)
      waitForOrderStatus(matcherNode, bobAssetIdRaw, id, "Accepted")
      id
    }

    def bobPlacesAssetOrder(bobCoinAmount: Int): String = {
      val bobOrder = if (twoAssetsPair.amountAsset.contains(bobAssetId)) {
        prepareOrder(bobNode, matcherNode, twoAssetsPair, OrderType.SELL, 1 * Order.PriceConstant, bobCoinAmount)
      } else {
        prepareOrder(bobNode, matcherNode, twoAssetsPair, OrderType.BUY, bobCoinAmount * Order.PriceConstant, 1)
      }
      val (id, _) = matcherPlaceOrder(matcherNode, bobOrder)
      waitForOrderStatus(matcherNode, bobAssetIdRaw, id, "Accepted")
      id
    }

    def waitOrderCancelled(orderId: String): Unit = {
      val task = matcherNode.waitFor[MatcherStatusResponse](s"Order '$orderId' is cancelled")(_.getOrderStatus(bobAssetIdRaw, orderId),
                                                                                              _.status == "Cancelled",
                                                                                              1.second)
      Await.result(task, 1.minute)
    }

    def checkBobOrderActive(orderId: String): Unit = {
      val newestOrderStatus = matcherNode.getOrderStatus(bobAssetIdRaw, orderId)
      Await.result(newestOrderStatus, 10.seconds).status shouldBe "Accepted"
    }
  }

  private def matcherExpectOrderPlacementRejected(order: Order, expectedStatusCode: Int, expectedStatus: String): Boolean = {
    val futureResult = matcherNode.expectIncorrectOrderPlacement(order, expectedStatusCode, expectedStatus)

    Await.result(futureResult, 1.minute)
  }

  private def matcherCancelOrder(node: Node, pair: AssetPair, orderId: String): String = {
    val privateKey    = node.privateKey
    val publicKey     = node.publicKey
    val request       = CancelOrderRequest(publicKey, ByteStr(Base58.decode(orderId).get), Array.emptyByteArray)
    val sig           = crypto.sign(privateKey, request.toSign)
    val signedRequest = request.copy(signature = sig)
    val futureResult  = matcherNode.cancelOrder(pair.amountAssetStr, pair.priceAssetStr, signedRequest)

    val result = Await.result(futureResult, 1.minute)

    result.status
  }

  private def createSignedTransferRequest(tx: TransferTransactionV1): SignedTransferV1Request = {
    import tx._
    SignedTransferV1Request(
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

  private def createSignedLeaseRequest(tx: LeaseTransactionV1): SignedLeaseV1Request = {
    import tx._
    SignedLeaseV1Request(
      Base58.encode(tx.sender.publicKey),
      amount,
      fee,
      recipient.stringRepr,
      timestamp,
      signature.base58
    )
  }

  private def createSignedLeaseCancelRequest(tx: LeaseCancelTransactionV1): SignedLeaseCancelV1Request = {
    import tx._
    SignedLeaseCancelV1Request(
      Base58.encode(tx.sender.publicKey),
      leaseId.base58,
      timestamp,
      signature.base58,
      fee
    )
  }

  private def transfer(from: Node, to: Node, assetId: Option[ByteStr], amount: Long, wait: Boolean = false): Unit = {
    val transferTx = TransferTransactionV1
      .selfSigned(
        assetId = assetId,
        sender = from.privateKey,
        recipient = scorex.account.Address.fromBytes(Base58.decode(to.address).get).explicitGet(),
        amount = amount,
        timestamp = System.currentTimeMillis(),
        feeAssetId = None,
        feeAmount = TransactionFee,
        attachment = Array.emptyByteArray
      )
      .right
      .get
    val tx = Await.result(matcherNode.signedTransfer(createSignedTransferRequest(transferTx)), 1.minute)
    if (wait) Await.result(matcherNode.waitForTransaction(tx.id), 1.minute)
  }

  private def lease(from: Node, to: Node, amount: Long): ByteStr = {
    val leaseTx = LeaseTransactionV1
      .selfSigned(
        sender = from.privateKey,
        recipient = scorex.account.Address.fromBytes(Base58.decode(to.address).get).explicitGet(),
        amount = amount,
        timestamp = System.currentTimeMillis(),
        fee = TransactionFee
      )
      .right
      .get
    val tx = Await.result(matcherNode.signedLease(createSignedLeaseRequest(leaseTx)), 1.minute)
    Await.result(matcherNode.waitForTransaction(tx.id), 1.minute)
    ByteStr(Base58.decode(tx.id).get)
  }

  private def cancelLease(sender: Node, leaseId: ByteStr, amount: Long): Unit = {
    val cancelLeaseTx = LeaseCancelTransactionV1
      .selfSigned(
        sender = sender.privateKey,
        leaseId = leaseId,
        fee = TransactionFee,
        timestamp = System.currentTimeMillis()
      )
      .right
      .get

    val tx = Await.result(matcherNode.signedLeaseCancel(createSignedLeaseCancelRequest(cancelLeaseTx)), 1.minute)
    Await.result(matcherNode.waitForTransaction(tx.id), 1.minute)
  }

}

object MatcherTestSuite {

  import ConfigFactory._
  import com.wavesplatform.it.NodeConfigs._

  private val ForbiddenAssetId = "FdbnAsset"
  private val AssetQuantity    = 1000
  private val MatcherFee       = 300000
  private val TransactionFee   = 300000
  private val Waves            = 100000000L

  private val minerDisabled = parseString("waves.miner.enable = no")
  private val matcherConfig = parseString(s"""
       |waves.matcher {
       |  enable = yes
       |  account = 3HmFkAoQRs4Y3PE2uR6ohN7wS4VqPBGKv7k
       |  bind-address = "0.0.0.0"
       |  order-match-tx-fee = 300000
       |  blacklisted-assets = ["$ForbiddenAssetId"]
       |  balance-watching.enable = yes
       |}""".stripMargin)

  private val Configs: Seq[Config] = (Default.last +: Random.shuffle(Default.init).take(3))
    .zip(Seq(matcherConfig, minerDisabled, minerDisabled, empty()))
    .map { case (n, o) => o.withFallback(n) }
}

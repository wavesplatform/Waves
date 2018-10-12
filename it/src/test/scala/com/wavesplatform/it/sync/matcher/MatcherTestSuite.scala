package com.wavesplatform.it.sync.matcher

import com.typesafe.config.Config
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.api.{AssetDecimalsInfo, LevelResponse}
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.duration._
import scala.util.Random
import com.wavesplatform.it.sync.matcher.config.MatcherDefaultConfig._

class MatcherTestSuite extends MatcherSuiteBase {
  private val aliceSellAmount                     = 500
  private val exTxFee                             = 300000
  private val amountAssetName                     = "AliceCoin"
  private val AssetQuantity                       = 1000
  private val aliceCoinDecimals: Byte             = 0
  override protected def nodeConfigs: Seq[Config] = Configs

  private def orderVersion = (Random.nextInt(2) + 1).toByte

  "Check cross ordering between Alice and Bob " - {
    // Alice issues new asset
    val aliceAsset = aliceNode
      .issue(aliceAcc.address, amountAssetName, "AliceCoin for matcher's tests", AssetQuantity, aliceCoinDecimals, reissuable = false, issueFee, 2)
      .id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)

    val aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    val order1 =
      matcherNode.placeOrder(aliceAcc, aliceWavesPair, OrderType.SELL, 2.waves * Order.PriceConstant, aliceSellAmount, orderVersion, 2.minutes)

    "assert addresses balances" in {
      aliceNode.assertAssetBalance(aliceAcc.address, aliceAsset, AssetQuantity)
      matcherNode.assertAssetBalance(matcherAcc.address, aliceAsset, 0)
      bobNode.assertAssetBalance(bobAcc.address, aliceAsset, 0)
    }

    "matcher should respond with Public key" in {
      matcherNode.matcherGet("/matcher").getResponseBody.stripPrefix("\"").stripSuffix("\"") shouldBe matcherNode.publicKeyStr
    }

    "get opened trading markets" in {
      val openMarkets = matcherNode.tradingMarkets()
      openMarkets.markets.size shouldBe 1
      val markets = openMarkets.markets.head

      markets.amountAssetName shouldBe amountAssetName
      markets.amountAssetInfo shouldBe Some(AssetDecimalsInfo(aliceCoinDecimals))

      markets.priceAssetName shouldBe "WAVES"
      markets.priceAssetInfo shouldBe Some(AssetDecimalsInfo(8))
    }

    "sell order could be placed correctly" - {
      "alice places sell order" in {
        order1.status shouldBe "OrderAccepted"

        // Alice checks that the order in order book
        matcherNode.waitOrderStatus(aliceWavesPair, order1.message.id, "Accepted")

        // Alice check that order is correct
        val orders = matcherNode.orderBook(aliceWavesPair)
        orders.asks.head.amount shouldBe aliceSellAmount
        orders.asks.head.price shouldBe 2.waves * Order.PriceConstant
      }

      "frozen amount should be listed via matcherBalance REST endpoint" in {
        matcherNode.reservedBalance(aliceAcc) shouldBe Map(aliceAsset -> aliceSellAmount)

        matcherNode.reservedBalance(bobAcc) shouldBe Map()
      }

      "and should be listed by trader's publiс key via REST" in {
        matcherNode.fullOrderHistory(aliceAcc).map(_.id) should contain(order1.message.id)
      }

      "and should match with buy order" in {
        val bobBalance     = bobNode.accountBalances(bobAcc.address)._1
        val matcherBalance = matcherNode.accountBalances(matcherNode.address)._1
        val aliceBalance   = aliceNode.accountBalances(aliceAcc.address)._1

        // Bob places a buy order
        val order2 = matcherNode.placeOrder(bobAcc, aliceWavesPair, OrderType.BUY, 2.waves * Order.PriceConstant, 200, orderVersion)
        order2.status shouldBe "OrderAccepted"

        matcherNode.waitOrderStatus(aliceWavesPair, order1.message.id, "PartiallyFilled")
        matcherNode.waitOrderStatus(aliceWavesPair, order2.message.id, "Filled")

        matcherNode.orderHistoryByPair(bobAcc, aliceWavesPair).map(_.id) should contain(order2.message.id)
        matcherNode.fullOrderHistory(bobAcc).map(_.id) should contain(order2.message.id)

        nodes.waitForHeightArise()

        // Bob checks that asset on his balance
        bobNode.assertAssetBalance(bobAcc.address, aliceAsset, 200)

        // Alice checks that part of her order still in the order book
        val orders = matcherNode.orderBook(aliceWavesPair)
        orders.asks.head.amount shouldBe 300
        orders.asks.head.price shouldBe 2.waves * Order.PriceConstant

        // Alice checks that she sold some assets
        aliceNode.assertAssetBalance(aliceAcc.address, aliceAsset, 800)

        // Bob checks that he spent some Waves
        val updatedBobBalance = bobNode.accountBalances(bobAcc.address)._1
        updatedBobBalance shouldBe (bobBalance - 2.waves * 200 - matcherFee)

        // Alice checks that she received some Waves
        val updatedAliceBalance = aliceNode.accountBalances(aliceAcc.address)._1
        updatedAliceBalance shouldBe (aliceBalance + 2.waves * 200 - (matcherFee * 200.0 / 500.0).toLong)

        // Matcher checks that it earn fees
        val updatedMatcherBalance = matcherNode.accountBalances(matcherNode.address)._1
        updatedMatcherBalance shouldBe (matcherBalance + matcherFee + (matcherFee * 200.0 / 500.0).toLong - exTxFee)
      }

      "request activeOnly orders" in {
        val aliceOrders = matcherNode.activeOrderHistory(aliceAcc)
        aliceOrders.map(_.id) shouldBe Seq(order1.message.id)
        val bobOrders = matcherNode.activeOrderHistory(bobAcc)
        bobOrders.map(_.id) shouldBe Seq()
      }

      "submitting sell orders should check availability of asset" in {
        // Bob trying to place order on more assets than he has - order rejected
        val badOrder =
          matcherNode.prepareOrder(bobAcc, aliceWavesPair, OrderType.SELL, (19.waves / 10.0 * Order.PriceConstant).toLong, 300, orderVersion)
        matcherNode.expectIncorrectOrderPlacement(badOrder, 400, "OrderRejected") should be(true)

        // Bob places order on available amount of assets - order accepted
        val order3 =
          matcherNode.placeOrder(bobAcc, aliceWavesPair, OrderType.SELL, (19.waves / 10.0 * Order.PriceConstant).toLong, 150, orderVersion)
        order3.status should be("OrderAccepted")

        // Bob checks that the order in the order book
        val orders = matcherNode.orderBook(aliceWavesPair)
        orders.asks should contain(LevelResponse(19.waves / 10 * Order.PriceConstant, 150))
      }

      "buy order should match on few price levels" in {
        val matcherBalance = matcherNode.accountBalances(matcherNode.address)._1
        val aliceBalance   = aliceNode.accountBalances(aliceAcc.address)._1
        val bobBalance     = bobNode.accountBalances(bobAcc.address)._1

        // Alice places a buy order
        val order4 =
          matcherNode.placeOrder(aliceAcc, aliceWavesPair, OrderType.BUY, (21.waves / 10.0 * Order.PriceConstant).toLong, 350, orderVersion)
        order4.status should be("OrderAccepted")

        // Where were 2 sells that should fulfill placed order
        matcherNode.waitOrderStatus(aliceWavesPair, order4.message.id, "Filled")

        // Check balances
        nodes.waitForHeightArise()
        aliceNode.assertAssetBalance(aliceAcc.address, aliceAsset, 950)
        bobNode.assertAssetBalance(bobAcc.address, aliceAsset, 50)

        val updatedMatcherBalance = matcherNode.accountBalances(matcherNode.address)._1
        updatedMatcherBalance should be(
          matcherBalance - 2 * exTxFee + matcherFee + (matcherFee * 150.0 / 350.0).toLong + (matcherFee * 200.0 / 350.0).toLong + (matcherFee * 200.0 / 500.0).toLong)

        val updatedBobBalance = bobNode.accountBalances(bobAcc.address)._1
        updatedBobBalance should be(bobBalance - matcherFee + 150 * (19.waves / 10.0).toLong)

        val updatedAliceBalance = aliceNode.accountBalances(aliceAcc.address)._1
        updatedAliceBalance should be(
          aliceBalance - (matcherFee * 200.0 / 350.0).toLong - (matcherFee * 150.0 / 350.0).toLong - (matcherFee * 200.0 / 500.0).toLong - (19.waves / 10.0).toLong * 150)
      }

      "order could be canceled and resubmitted again" in {
        // Alice cancels the very first order (100 left)
        val status1 = matcherNode.cancelOrder(aliceAcc, aliceWavesPair, Some(order1.message.id))
        status1.status should be("OrderCanceled")

        // Alice checks that the order book is empty
        val orders1 = matcherNode.orderBook(aliceWavesPair)
        orders1.asks.size should be(0)
        orders1.bids.size should be(0)

        // Alice places a new sell order on 100
        val order4 =
          matcherNode.placeOrder(aliceAcc, aliceWavesPair, OrderType.SELL, 2.waves * Order.PriceConstant, 100, orderVersion)
        order4.status should be("OrderAccepted")

        // Alice checks that the order is in the order book
        val orders2 = matcherNode.orderBook(aliceWavesPair)
        orders2.asks should contain(LevelResponse(20.waves / 10 * Order.PriceConstant, 100))
      }

      "buy order should execute all open orders and put remaining in order book" in {
        val matcherBalance = matcherNode.accountBalances(matcherNode.address)._1
        val aliceBalance   = aliceNode.accountBalances(aliceAcc.address)._1
        val bobBalance     = bobNode.accountBalances(bobAcc.address)._1

        // Bob places buy order on amount bigger then left in sell orders
        val order5 = matcherNode.placeOrder(bobAcc, aliceWavesPair, OrderType.BUY, 2.waves * Order.PriceConstant, 130, orderVersion)
        order5.status should be("OrderAccepted")

        // Check that the order is partially filled
        matcherNode.waitOrderStatus(aliceWavesPair, order5.message.id, "PartiallyFilled")

        // Check that remaining part of the order is in the order book
        val orders = matcherNode.orderBook(aliceWavesPair)
        orders.bids should contain(LevelResponse(2.waves * Order.PriceConstant, 30))

        // Check balances
        nodes.waitForHeightArise()
        aliceNode.assertAssetBalance(aliceAcc.address, aliceAsset, 850)
        bobNode.assertAssetBalance(bobAcc.address, aliceAsset, 150)

        val updatedMatcherBalance = matcherNode.accountBalances(matcherNode.address)._1
        updatedMatcherBalance should be(matcherBalance - exTxFee + matcherFee + (matcherFee * 100.0 / 130.0).toLong)

        val updatedBobBalance = bobNode.accountBalances(bobAcc.address)._1
        updatedBobBalance should be(bobBalance - (matcherFee * 100.0 / 130.0).toLong - 100 * 2.waves)

        val updatedAliceBalance = aliceNode.accountBalances(aliceAcc.address)._1
        updatedAliceBalance should be(aliceBalance - matcherFee + 2.waves * 100)
      }

      "market status" in {
        val resp = matcherNode.marketStatus(aliceWavesPair)

        resp.lastPrice shouldBe Some(2.waves * Order.PriceConstant)
        resp.lastSide shouldBe Some("buy") // Same type as order5
        resp.bid shouldBe Some(2.waves * Order.PriceConstant)
        resp.bidAmount shouldBe Some(30)
        resp.ask shouldBe None
        resp.askAmount shouldBe None
      }

      "request order book for blacklisted pair" in {
        val f = matcherNode.matcherGetStatusCode(s"/matcher/orderbook/$ForbiddenAssetId/WAVES", 404)
        f.message shouldBe s"Invalid Asset ID: $ForbiddenAssetId"
      }

      "should consider UTX pool when checking the balance" in {
        // Bob issues new asset
        val bobAssetQuantity = 10000
        val bobAssetName     = "BobCoin"
        val bobAsset         = bobNode.issue(bobAcc.address, bobAssetName, "Bob's asset", bobAssetQuantity, 0, false, issueFee, 2).id
        nodes.waitForHeightAriseAndTxPresent(bobAsset)

        aliceNode.assertAssetBalance(aliceAcc.address, bobAsset, 0)
        matcherNode.assertAssetBalance(matcherAcc.address, bobAsset, 0)
        bobNode.assertAssetBalance(bobAcc.address, bobAsset, bobAssetQuantity)
        val bobWavesPair = AssetPair(ByteStr.decodeBase58(bobAsset).toOption, None)

        def bobOrder = matcherNode.prepareOrder(bobAcc, bobWavesPair, OrderType.SELL, 1.waves * Order.PriceConstant, bobAssetQuantity, orderVersion)

        val order6 = matcherNode.placeOrder(bobOrder)
        matcherNode.waitOrderStatus(bobWavesPair, order6.message.id, "Accepted")

        // Alice wants to buy all Bob's assets for 1 Wave
        val order7 =
          matcherNode.placeOrder(aliceAcc, bobWavesPair, OrderType.BUY, 1.waves * Order.PriceConstant, bobAssetQuantity, orderVersion)
        matcherNode.waitOrderStatus(bobWavesPair, order7.message.id, "Filled")

        // Bob tries to do the same operation, but at now he have no assets
        matcherNode.expectIncorrectOrderPlacement(bobOrder, 400, "OrderRejected")
      }

      "trader can buy waves for assets with order without having waves" in {
        // Bob issues new asset
        val bobAssetQuantity = 10000
        val bobAssetName     = "BobCoin2"
        val bobAsset         = bobNode.issue(bobAcc.address, bobAssetName, "Bob's asset", bobAssetQuantity, 0, false, issueFee, 2).id
        nodes.waitForHeightAriseAndTxPresent(bobAsset)

        val bobWavesPair = AssetPair(
          amountAsset = ByteStr.decodeBase58(bobAsset).toOption,
          priceAsset = None
        )

        aliceNode.assertAssetBalance(aliceAcc.address, bobAsset, 0)
        matcherNode.assertAssetBalance(matcherAcc.address, bobAsset, 0)
        bobNode.assertAssetBalance(bobAcc.address, bobAsset, bobAssetQuantity)

        // Bob wants to sell all own assets for 1 Wave
        def bobOrder = matcherNode.prepareOrder(bobAcc, bobWavesPair, OrderType.SELL, 1.waves * Order.PriceConstant, bobAssetQuantity, orderVersion)

        val order8 = matcherNode.placeOrder(bobOrder)
        matcherNode.waitOrderStatus(bobWavesPair, order8.message.id, "Accepted")

        // Bob moves all waves to Alice
        val h1              = matcherNode.height
        val bobBalance      = matcherNode.accountBalances(bobAcc.address)._1
        val transferAmount  = bobBalance - minFee
        val transferAliceId = bobNode.transfer(bobAcc.address, aliceAcc.address, transferAmount, minFee, None, None, 2).id
        nodes.waitForHeightAriseAndTxPresent(transferAliceId)

        matcherNode.accountBalances(bobAcc.address)._1 shouldBe 0

        // Order should stay accepted
        matcherNode.waitForHeight(h1 + 5, 2.minutes)
        matcherNode.waitOrderStatus(bobWavesPair, order8.message.id, "Accepted")

        // Cleanup
        nodes.waitForHeightArise()
        matcherNode.cancelOrder(bobAcc, bobWavesPair, Some(order8.message.id)).status should be("OrderCanceled")

        val transferBobId = aliceNode.transfer(aliceAcc.address, bobAcc.address, transferAmount, minFee, None, None, 2).id
        nodes.waitForHeightAriseAndTxPresent(transferBobId)
      }
    }

    "batch cancel" ignore {
      val ordersNum = 5
      def fileOrders(n: Int, pair: AssetPair): Seq[String] = 0 until n map { i =>
        val o =
          matcherNode.placeOrder(matcherNode.prepareOrder(aliceAcc, pair, OrderType.BUY, 1.waves * Order.PriceConstant, 100, (1 + (i & 1)).toByte))
        o.status should be("OrderAccepted")
        o.message.id
      }

      val asset2 =
        aliceNode.issue(aliceAcc.address, "AliceCoin2", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, issueFee, 2).id
      nodes.waitForHeightAriseAndTxPresent(asset2)
      val aliceWavesPair2 = AssetPair(ByteStr.decodeBase58(asset2).toOption, None)

      "canceling an order doesn't affect other orders for the same pair" in {
        val orders                          = fileOrders(ordersNum, aliceWavesPair)
        val (orderToCancel, ordersToRetain) = (orders.head, orders.tail)

        val cancel = matcherNode.cancelOrder(aliceAcc, aliceWavesPair, Some(orderToCancel))
        cancel.status should be("OrderCanceled")

        ordersToRetain foreach {
          matcherNode.waitOrderStatus(aliceWavesPair, _, "Accepted")
        }
      }

      "cancel orders by pair" ignore {
        val ordersToCancel = fileOrders(orderLimit + ordersNum, aliceWavesPair)
        val ordersToRetain = fileOrders(ordersNum, aliceWavesPair2)
        val ts             = Some(System.currentTimeMillis)

        val cancel = matcherNode.cancelOrder(aliceAcc, aliceWavesPair, None, ts)
        cancel.status should be("Cancelled")

        ordersToCancel foreach {
          matcherNode.waitOrderStatus(aliceWavesPair, _, "Cancelled")
        }
        ordersToRetain foreach {
          matcherNode.waitOrderStatus(aliceWavesPair2, _, "Accepted")
        }

        // signed timestamp is mandatory
        assertBadRequestAndMessage(matcherNode.cancelOrder(aliceAcc, aliceWavesPair, None, None), "invalid signature")

        // timestamp reuse shouldn't be allowed
        assertBadRequest(matcherNode.cancelOrder(aliceAcc, aliceWavesPair, None, ts))
      }

      "cancel all orders" ignore {
        val orders1 = fileOrders(orderLimit + ordersNum, aliceWavesPair)
        val orders2 = fileOrders(orderLimit + ordersNum, aliceWavesPair2)
        val ts      = Some(System.currentTimeMillis)

        val cancel = matcherNode.cancelAllOrders(aliceAcc, ts)
        cancel.status should be("Cancelled")

        orders1 foreach {
          matcherNode.waitOrderStatus(aliceWavesPair, _, "Cancelled")
        }
        orders2 foreach {
          matcherNode.waitOrderStatus(aliceWavesPair2, _, "Cancelled")
        }

        // signed timestamp is mandatory
        assertBadRequestAndMessage(matcherNode.cancelAllOrders(aliceAcc, None), "invalid signature")

        // timestamp reuse shouldn't be allowed
        assertBadRequest(matcherNode.cancelAllOrders(aliceAcc, ts))
      }
    }
  }
}

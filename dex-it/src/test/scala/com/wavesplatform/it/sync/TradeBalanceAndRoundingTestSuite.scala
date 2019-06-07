package com.wavesplatform.it.sync

import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.AssetDecimalsInfo
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.matcher.model.LimitOrder
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}

import scala.concurrent.duration._
import scala.math.BigDecimal.RoundingMode

class TradeBalanceAndRoundingTestSuite extends MatcherSuiteBase {
  {
    val xs = Seq(IssueUsdTx, IssueEthTx, IssueWctTx).map(_.json()).map(node.broadcastRequest(_))
    xs.foreach(x => node.waitForTransaction(x.id))
  }

  "Alice and Bob trade WAVES-USD" - {
    val aliceWavesBalanceBefore = node.accountBalances(alice.address)._1
    val bobWavesBalanceBefore   = node.accountBalances(bob.address)._1

    val price           = 238
    val buyOrderAmount  = 425532L
    val sellOrderAmount = 3100000000L

    val correctedSellAmount = correctAmount(sellOrderAmount, price)

    val adjustedAmount = receiveAmount(OrderType.BUY, buyOrderAmount, price)
    val adjustedTotal  = receiveAmount(OrderType.SELL, buyOrderAmount, price)

    log.debug(s"correctedSellAmount: $correctedSellAmount, adjustedAmount: $adjustedAmount, adjustedTotal: $adjustedTotal")

    "place usd-waves order" in {
      // Alice wants to sell USD for Waves
      val bobOrder1   = node.prepareOrder(bob, wavesUsdPair, OrderType.SELL, sellOrderAmount, price)
      val bobOrder1Id = node.placeOrder(bobOrder1).message.id
      node.waitOrderStatus(wavesUsdPair, bobOrder1Id, "Accepted", 1.minute)
      node.reservedBalance(bob)("WAVES") shouldBe sellOrderAmount + matcherFee
      node.tradableBalance(bob, wavesUsdPair)("WAVES") shouldBe bobWavesBalanceBefore - (sellOrderAmount + matcherFee)

      val aliceOrder   = node.prepareOrder(alice, wavesUsdPair, OrderType.BUY, buyOrderAmount, price)
      val aliceOrderId = node.placeOrder(aliceOrder).message.id
      node.waitOrderStatusAndAmount(wavesUsdPair, aliceOrderId, "Filled", Some(420169L), 1.minute)

      // Bob wants to buy some USD
      node.waitOrderStatusAndAmount(wavesUsdPair, bobOrder1Id, "PartiallyFilled", Some(420169L), 1.minute)

      // Each side get fair amount of assets
      node.waitOrderInBlockchain(aliceOrder.idStr())
    }

    "get opened trading markets. USD price-asset" in {
      val openMarkets = node.tradingMarkets()
      openMarkets.markets.size shouldBe 1
      val markets = openMarkets.markets.head

      markets.amountAssetName shouldBe "WAVES"
      markets.amountAssetInfo shouldBe Some(AssetDecimalsInfo(8))

      markets.priceAssetName shouldBe usdAssetName
      markets.priceAssetInfo shouldBe Some(AssetDecimalsInfo(Decimals))
    }

    "check usd and waves balance after fill" in {
      val aliceWavesBalanceAfter = node.accountBalances(alice.address)._1
      val aliceUsdBalance = node.assetBalance(alice.address, UsdId.toString).balance

      val bobWavesBalanceAfter = node.accountBalances(bob.address)._1
      val bobUsdBalance = node.assetBalance(bob.address, UsdId.toString).balance

      (aliceWavesBalanceAfter - aliceWavesBalanceBefore) should be(
        adjustedAmount - (BigInt(matcherFee) * adjustedAmount / buyOrderAmount).bigInteger.longValue())

      aliceUsdBalance - defaultAssetQuantity should be(-adjustedTotal)
      bobWavesBalanceAfter - bobWavesBalanceBefore should be(
        -adjustedAmount - (BigInt(matcherFee) * adjustedAmount / sellOrderAmount).bigInteger.longValue())
      bobUsdBalance should be(adjustedTotal)
    }

    "check filled amount and tradable balance" in {
      val bobsOrderId  = node.fullOrderHistory(bob).head.id
      val filledAmount = node.orderStatus(bobsOrderId, wavesUsdPair).filledAmount.getOrElse(0L)

      filledAmount shouldBe adjustedAmount
    }

    "check reserved balance" in {
      val reservedFee = BigInt(matcherFee) - (BigInt(matcherFee) * adjustedAmount / sellOrderAmount)
      log.debug(s"reservedFee: $reservedFee")
      val expectedBobReservedBalance = correctedSellAmount - adjustedAmount + reservedFee
      node.reservedBalance(bob)("WAVES") shouldBe expectedBobReservedBalance

      node.reservedBalance(alice) shouldBe empty
    }

    "check waves-usd tradable balance" in {
      val orderHistory = node.fullOrderHistory(bob)
      orderHistory.size should be(1)

      val expectedBobTradableBalance = bobWavesBalanceBefore - (correctedSellAmount + matcherFee)
      node.tradableBalance(bob, wavesUsdPair)("WAVES") shouldBe expectedBobTradableBalance
      node.tradableBalance(alice, wavesUsdPair)("WAVES") shouldBe node.accountBalances(alice.address)._1

      val orderId = orderHistory.head.id
      node.cancelOrder(bob, wavesUsdPair, orderId)
      node.waitOrderStatus(wavesUsdPair, orderId, "Cancelled", 1.minute)
      node.tradableBalance(bob, wavesUsdPair)("WAVES") shouldBe node.accountBalances(bob.address)._1
    }
  }

  "Alice and Bob trade WAVES-USD check CELLING" - {
    val price2           = 289
    val buyOrderAmount2  = 0.07.waves
    val sellOrderAmount2 = 3.waves

    val correctedSellAmount2 = correctAmount(sellOrderAmount2, price2)

    "place usd-waves order" in {
      // Alice wants to sell USD for Waves
      val bobWavesBalanceBefore = node.accountBalances(bob.address)._1
      node.tradableBalance(bob, wavesUsdPair)("WAVES")
      val bobOrder1   = node.prepareOrder(bob, wavesUsdPair, OrderType.SELL, sellOrderAmount2, price2)
      val bobOrder1Id = node.placeOrder(bobOrder1).message.id
      node.waitOrderStatus(wavesUsdPair, bobOrder1Id, "Accepted", 1.minute)

      node.reservedBalance(bob)("WAVES") shouldBe correctedSellAmount2 + matcherFee
      node.tradableBalance(bob, wavesUsdPair)("WAVES") shouldBe bobWavesBalanceBefore - (correctedSellAmount2 + matcherFee)

      val aliceOrder   = node.prepareOrder(alice, wavesUsdPair, OrderType.BUY, buyOrderAmount2, price2)
      val aliceOrderId = node.placeOrder(aliceOrder).message.id
      node.waitOrderStatus(wavesUsdPair, aliceOrderId, "Filled", 1.minute)

      // Bob wants to buy some USD
      node.waitOrderStatus(wavesUsdPair, bobOrder1Id, "PartiallyFilled", 1.minute)

      // Each side get fair amount of assets
      node.waitOrderInBlockchain(aliceOrder.idStr())
      node.cancelOrder(bob, wavesUsdPair, bobOrder1Id)
    }

  }

  "Alice and Bob trade WCT-USD sell price less than buy price" - {
    "place wcd-usd order corrected by new price sell amount less then initial one" in {
      val buyPrice   = 247700
      val sellPrice  = 135600
      val buyAmount  = 46978
      val sellAmount = 56978

      val bobOrderId = node.placeOrder(bob, wctUsdPair, SELL, sellAmount, sellPrice, matcherFee).message.id
      node.waitOrderStatus(wctUsdPair, bobOrderId, "Accepted", 1.minute)
      val aliceOrderId = node.placeOrder(alice, wctUsdPair, BUY, buyAmount, buyPrice, matcherFee).message.id
      node.waitOrderStatus(wctUsdPair, aliceOrderId, "Filled", 1.minute)

      node.waitOrderInBlockchain(aliceOrderId)
      node.cancelOrder(bob, wctUsdPair, bobOrderId)

      node.waitOrderStatus(wctUsdPair, bobOrderId, "Cancelled", 1.minute)

      node.reservedBalance(bob) shouldBe empty
      node.reservedBalance(alice) shouldBe empty
    }
  }

  "Alice and Bob trade WCT-USD 1" - {
    val wctUsdSellAmount = 347
    val wctUsdBuyAmount  = 146
    val wctUsdPrice      = 12739213

    "place wct-usd order" in {
      val aliceUsdBalance = node.assetBalance(alice.address, UsdId.toString).balance
      val bobUsdBalance = node.assetBalance(bob.address, UsdId.toString).balance
      val bobWctInitBalance = node.assetBalance(bob.address, WctId.toString).balance

      val bobOrderId =
        node.placeOrder(bob, wctUsdPair, SELL, wctUsdSellAmount, wctUsdPrice, matcherFee).message.id
      node.waitOrderStatus(wctUsdPair, bobOrderId, "Accepted", 1.minute)

      val aliceOrderId =
        node.placeOrder(alice, wctUsdPair, BUY, wctUsdBuyAmount, wctUsdPrice, matcherFee).message.id
      node.waitOrderStatus(wctUsdPair, aliceOrderId, "Filled", 1.minute)

      node.waitOrderInBlockchain(aliceOrderId)

      val executedAmount         = correctAmount(wctUsdBuyAmount, wctUsdPrice) // 142
      val bobReceiveUsdAmount    = receiveAmount(SELL, wctUsdBuyAmount, wctUsdPrice)
      val expectedReservedBobWct = wctUsdSellAmount - executedAmount // 205 = 347 - 142

      node.reservedBalance(bob)(s"$WctId") shouldBe expectedReservedBobWct
      // 999999999652 = 999999999999 - 142 - 205
      node.tradableBalance(bob, wctUsdPair)(s"$WctId") shouldBe bobWctInitBalance - executedAmount - expectedReservedBobWct
      node.tradableBalance(bob, wctUsdPair)(s"$UsdId") shouldBe bobUsdBalance + bobReceiveUsdAmount

      node.reservedBalance(alice) shouldBe empty
      node.tradableBalance(alice, wctUsdPair)(s"$UsdId") shouldBe aliceUsdBalance - bobReceiveUsdAmount

      val expectedReservedWaves = matcherFee - LimitOrder.partialFee(matcherFee, wctUsdSellAmount, executedAmount)
      node.reservedBalance(bob)("WAVES") shouldBe expectedReservedWaves

      node.cancelOrder(bob, wctUsdPair, node.fullOrderHistory(bob).head.id)
    }

    "reserved balance is empty after the total execution" in {
      val aliceOrderId = node.placeOrder(alice, wctUsdPair, BUY, 5000000, 100000, matcherFee).message.id
      node.waitOrderStatus(wctUsdPair, aliceOrderId, "Accepted", 1.minute)

      val bobOrderId = node.placeOrder(bob, wctUsdPair, SELL, 5000000, 99908, matcherFee).message.id
      node.waitOrderStatus(wctUsdPair, bobOrderId, "Filled", 1.minute)
      node.waitOrderStatus(wctUsdPair, aliceOrderId, "Filled", 1.minute)

      node.waitOrderInBlockchain(bobOrderId)
      node.reservedBalance(alice) shouldBe empty
      node.reservedBalance(bob) shouldBe empty
    }

  }

  "get opened trading markets. Check WCT-USD" in {
    val openMarkets = node.tradingMarkets()
    val markets     = openMarkets.markets.last

    markets.amountAssetName shouldBe wctAssetName
    markets.amountAssetInfo shouldBe Some(AssetDecimalsInfo(Decimals))

    markets.priceAssetName shouldBe usdAssetName
    markets.priceAssetInfo shouldBe Some(AssetDecimalsInfo(Decimals))
  }

  "Alice and Bob trade WCT-WAVES on not enough fee when place order" - {
    val wctWavesSellAmount = 2
    val wctWavesPrice      = 11234560000000L

    "bob lease all waves exact half matcher fee" in {
      val leasingAmount = node.accountBalances(bob.address)._1 - leasingFee - matcherFee / 2
      val leaseTxId     = node.broadcastLease(bob, matcher.address, leasingAmount, leasingFee, waitForTx = true).id
      val bobOrderId =
        node.placeOrder(bob, wctWavesPair, SELL, wctWavesSellAmount, wctWavesPrice, matcherFee).message.id
      node.waitOrderStatus(wctWavesPair, bobOrderId, "Accepted", 1.minute)

      node.tradableBalance(bob, wctWavesPair)("WAVES") shouldBe matcherFee / 2 + receiveAmount(SELL, wctWavesSellAmount, wctWavesPrice) - matcherFee
      node.cancelOrder(bob, wctWavesPair, bobOrderId)

      assertBadRequestAndResponse(
        node.placeOrder(bob, wctWavesPair, SELL, wctWavesSellAmount / 2, wctWavesPrice, matcherFee),
        "Not enough tradable balance"
      )

      node.broadcastCancelLease(bob, leaseTxId, leasingFee, waitForTx = true)
    }
  }

  "Alice and Bob trade ETH-WAVES" - {
    "reserved balance is empty after the total execution" in {
      val counterId1 = node.placeOrder(alice, ethWavesPair, SELL, 2864310, 300000, matcherFee).message.id
      node.waitOrderStatus(ethWavesPair, counterId1, "Accepted", 1.minute)

      val counterId2 = node.placeOrder(alice, ethWavesPair, SELL, 7237977, 300000, matcherFee).message.id
      node.waitOrderStatus(ethWavesPair, counterId2, "Accepted", 1.minute)

      val submittedId = node.placeOrder(bob, ethWavesPair, BUY, 4373667, 300000, matcherFee).message.id

      node.waitOrderStatus(ethWavesPair, counterId1, "Filled", 1.minute)
      node.waitOrderStatus(ethWavesPair, counterId2, "PartiallyFilled", 1.minute)
      node.waitOrderStatus(ethWavesPair, submittedId, "Filled", 1.minute)

      node.waitOrderInBlockchain(submittedId)
      node.reservedBalance(bob) shouldBe empty
      node.cancelOrder(alice, ethWavesPair, counterId2)
    }
  }

  "Submitted order Canceled during match" in {
    val bobOrder   = node.prepareOrder(matcher, wavesUsdPair, OrderType.SELL, 10000000L, 10L)
    val bobOrderId = node.placeOrder(bobOrder).message.id
    node.waitOrderStatus(wavesUsdPair, bobOrderId, "Accepted", 1.minute)

    val aliceOrder   = node.prepareOrder(alice, wavesUsdPair, OrderType.BUY, 100000L, 1000L)
    val aliceOrderId = node.placeOrder(aliceOrder).message.id

    node.waitOrderStatusAndAmount(wavesUsdPair, aliceOrderId, "Cancelled", Some(0), 1.minute)

    withClue("Alice's reserved balance:") {
      node.reservedBalance(alice) shouldBe empty
    }

    val aliceOrders = node.ordersByAddress(alice, activeOnly = false, 1.minute)
    aliceOrders should not be empty

    val order = aliceOrders.find(_.id == aliceOrderId).getOrElse(throw new IllegalStateException(s"Alice should have the $aliceOrderId order"))
    order.status shouldBe "Cancelled"

    node.cancelOrder(matcher, wavesUsdPair, bobOrderId)
  }

  def correctAmount(a: Long, price: Long): Long = {
    val settledTotal = (BigDecimal(price) * a / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
    (BigDecimal(settledTotal) / price * Order.PriceConstant).setScale(0, RoundingMode.CEILING).toLong
  }

  def receiveAmount(ot: OrderType, matchAmount: Long, matchPrice: Long): Long =
    if (ot == BUY) correctAmount(matchAmount, matchPrice)
    else {
      (BigInt(matchAmount) * matchPrice / Order.PriceConstant).bigInteger.longValueExact()
    }

}

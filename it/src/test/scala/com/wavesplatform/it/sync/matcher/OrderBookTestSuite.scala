package com.wavesplatform.it.sync.matcher

import com.typesafe.config.Config
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
import com.wavesplatform.transaction.assets.exchange.OrderType._

class OrderBookTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = Configs

  Seq(IssueUsdTx, IssueWctTx).map(createSignedIssueRequest).map(matcherNode.signedIssue).foreach { tx =>
    nodes.waitForTransaction(tx.id)
  }

  Seq(
    aliceNode.transfer(IssueUsdTx.sender.toAddress.stringRepr, aliceAcc.address, defaultAssetQuantity, 100000, Some(UsdId.toString), None, 2),
    bobNode.transfer(IssueWctTx.sender.toAddress.stringRepr, bobAcc.address, defaultAssetQuantity, 100000, Some(WctId.toString), None, 2)
  ).foreach { tx =>
    nodes.waitForTransaction(tx.id)
  }

  case class ReservedBalances(wct: Long, usd: Long, waves: Long)
  def reservedBalancesOf(pk: PrivateKeyAccount): ReservedBalances = {
    val reservedBalances = matcherNode.reservedBalance(pk)
    ReservedBalances(
      reservedBalances.getOrElse(WctId.toString, 0),
      reservedBalances.getOrElse(UsdId.toString, 0),
      reservedBalances.getOrElse("WAVES", 0)
    )
  }

  val (amount, price) = (1000L, PriceConstant)

  "When delete order book" - {
    val buyOrder        = matcherNode.placeOrder(aliceAcc, wctUsdPair, BUY, 2 * amount, price, matcherFee).message.id
    val anotherBuyOrder = matcherNode.placeOrder(aliceAcc, wctUsdPair, BUY, amount, price, matcherFee).message.id

    val submitted = matcherNode.placeOrder(bobAcc, wctUsdPair, SELL, amount, price, matcherFee).message.id

    val sellOrder = matcherNode.placeOrder(bobAcc, wctUsdPair, SELL, amount, 2 * price, matcherFee).message.id

    matcherNode.waitOrderStatus(wctUsdPair, buyOrder, "PartiallyFilled")
    matcherNode.waitOrderStatus(wctUsdPair, submitted, "Filled")

    val (aliceRBForOnePair, bobRBForOnePair) = (reservedBalancesOf(aliceAcc), reservedBalancesOf(bobAcc))

    val buyOrderForAnotherPair = matcherNode.placeOrder(aliceAcc, wctWavesPair, BUY, amount, price, matcherFee).message.id
    val sellOrderForAnotherPair =
      matcherNode.placeOrder(bobAcc, wctWavesPair, SELL, amount, 2 * price, matcherFee).message.id

    matcherNode.waitOrderStatus(wctWavesPair, buyOrderForAnotherPair, "Accepted")
    matcherNode.waitOrderStatus(wctWavesPair, sellOrderForAnotherPair, "Accepted")

    val (aliceRBForBothPairs, bobRBForBothPairs) = (reservedBalancesOf(aliceAcc), reservedBalancesOf(bobAcc))

    val marketStatusBeforeDeletion = matcherNode.marketStatus(wctUsdPair)

    matcherNode.deleteOrderBook(wctUsdPair)

    "orders by the pair should be canceled" in {
      matcherNode.waitOrderStatus(wctUsdPair, buyOrder, "Cancelled")
      matcherNode.waitOrderStatus(wctUsdPair, anotherBuyOrder, "Cancelled")
      matcherNode.waitOrderStatus(wctUsdPair, sellOrder, "Cancelled")
    }

    "orderbook was really deleted" in {
      val orderBook = matcherNode.orderBook(wctUsdPair)
      orderBook.bids shouldBe empty
      orderBook.asks shouldBe empty
    }

    "reserved balances should be released for the pair" in {
      val (aliceReservedBalances, bobReservedBalances) = (reservedBalancesOf(aliceAcc), reservedBalancesOf(bobAcc))
      aliceReservedBalances.usd shouldBe 0
      aliceReservedBalances.waves shouldBe (aliceRBForBothPairs.waves - aliceRBForOnePair.waves)
      bobReservedBalances.wct shouldBe (bobRBForBothPairs.wct - bobRBForOnePair.wct)
      bobReservedBalances.waves shouldBe (bobRBForBothPairs.waves - bobRBForOnePair.waves)
    }

    "it should not affect other pairs and their orders" in {
      matcherNode.orderStatus(buyOrderForAnotherPair, wctWavesPair).status shouldBe "Accepted"
      matcherNode.orderStatus(sellOrderForAnotherPair, wctWavesPair).status shouldBe "Accepted"

      val orderBook = matcherNode.orderBook(wctWavesPair)
      orderBook.bids shouldNot be(empty)
      orderBook.asks shouldNot be(empty)
    }

    "it should not affect market status" in {
      matcherNode.marketStatus(wctUsdPair) shouldEqual marketStatusBeforeDeletion
    }
  }

}

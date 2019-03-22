package com.wavesplatform.it.sync

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
import com.wavesplatform.transaction.assets.exchange.OrderType._

class OrderBookTestSuite extends MatcherSuiteBase {

  {
    val xs = Seq(IssueUsdTx, IssueWctTx).map(_.json()).map(node.broadcastRequest(_))
    xs.foreach(x => node.waitForTransaction(x.id))
  }

  case class ReservedBalances(wct: Long, usd: Long, waves: Long)
  def reservedBalancesOf(pk: PrivateKeyAccount): ReservedBalances = {
    val reservedBalances = node.reservedBalance(pk)
    ReservedBalances(
      reservedBalances.getOrElse(WctId.toString, 0),
      reservedBalances.getOrElse(UsdId.toString, 0),
      reservedBalances.getOrElse("WAVES", 0)
    )
  }

  val (amount, price) = (1000L, PriceConstant)

  "When delete order book" - {
    val buyOrder        = node.placeOrder(alice, wctUsdPair, BUY, 2 * amount, price, matcherFee).message.id
    val anotherBuyOrder = node.placeOrder(alice, wctUsdPair, BUY, amount, price, matcherFee).message.id

    val submitted = node.placeOrder(bob, wctUsdPair, SELL, amount, price, matcherFee).message.id

    val sellOrder = node.placeOrder(bob, wctUsdPair, SELL, amount, 2 * price, matcherFee).message.id

    node.waitOrderStatus(wctUsdPair, buyOrder, "PartiallyFilled")
    node.waitOrderStatus(wctUsdPair, submitted, "Filled")

    val (aliceRBForOnePair, bobRBForOnePair) = (reservedBalancesOf(alice), reservedBalancesOf(bob))

    val buyOrderForAnotherPair = node.placeOrder(alice, wctWavesPair, BUY, amount, price, matcherFee).message.id
    val sellOrderForAnotherPair =
      node.placeOrder(bob, wctWavesPair, SELL, amount, 2 * price, matcherFee).message.id

    node.waitOrderStatus(wctWavesPair, buyOrderForAnotherPair, "Accepted")
    node.waitOrderStatus(wctWavesPair, sellOrderForAnotherPair, "Accepted")

    val (aliceRBForBothPairs, bobRBForBothPairs) = (reservedBalancesOf(alice), reservedBalancesOf(bob))

    node.deleteOrderBook(wctUsdPair)

    "orders by the pair should be canceled" in {
      node.waitOrderStatus(wctUsdPair, buyOrder, "Cancelled")
      node.waitOrderStatus(wctUsdPair, anotherBuyOrder, "Cancelled")
      node.waitOrderStatus(wctUsdPair, sellOrder, "Cancelled")
    }

    "orderbook was really deleted" in {
      val orderBook = node.orderBook(wctUsdPair)
      orderBook.bids shouldBe empty
      orderBook.asks shouldBe empty
    }

    "reserved balances should be released for the pair" in {
      val (aliceReservedBalances, bobReservedBalances) = (reservedBalancesOf(alice), reservedBalancesOf(bob))
      aliceReservedBalances.usd shouldBe 0
      aliceReservedBalances.waves shouldBe (aliceRBForBothPairs.waves - aliceRBForOnePair.waves)
      bobReservedBalances.wct shouldBe (bobRBForBothPairs.wct - bobRBForOnePair.wct)
      bobReservedBalances.waves shouldBe (bobRBForBothPairs.waves - bobRBForOnePair.waves)
    }

    "it should not affect other pairs and their orders" in {
      node.orderStatus(buyOrderForAnotherPair, wctWavesPair).status shouldBe "Accepted"
      node.orderStatus(sellOrderForAnotherPair, wctWavesPair).status shouldBe "Accepted"

      val orderBook = node.orderBook(wctWavesPair)
      orderBook.bids shouldNot be(empty)
      orderBook.asks shouldNot be(empty)
    }
  }
}

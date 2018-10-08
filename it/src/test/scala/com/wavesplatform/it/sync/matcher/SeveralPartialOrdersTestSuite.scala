package com.wavesplatform.it.sync.matcher

import com.typesafe.config.Config
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync.matcherFee
import com.wavesplatform.transaction.assets.exchange.OrderType.BUY
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}
import scala.concurrent.duration._
import scala.math.BigDecimal.RoundingMode
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._

class SeveralPartialOrdersTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = Configs

  matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
  nodes.waitForHeightArise()

  aliceNode.transfer(aliceNode.address, aliceAcc.address, defaultAssetQuantity, 100000, Some(UsdId.toString), None, 2)
  nodes.waitForHeightArise()

  "Alice and Bob trade WAVES-USD" - {
    nodes.waitForHeightArise()
    val bobWavesBalanceBefore = matcherNode.accountBalances(bobAcc.address)._1

    val price           = 238
    val buyOrderAmount  = 425532L
    val sellOrderAmount = 840340L

    "place usd-waves order" in {
      // Alice wants to sell USD for Waves

      val bobOrder   = matcherNode.prepareOrder(bobAcc, wavesUsdPair, OrderType.SELL, price, sellOrderAmount)
      val bobOrderId = matcherNode.placeOrder(bobOrder).message.id
      matcherNode.waitOrderStatus(wavesUsdPair, bobOrderId, "Accepted", 1.minute)
      matcherNode.reservedBalance(bobAcc)("WAVES") shouldBe sellOrderAmount + matcherFee
      matcherNode.tradableBalance(bobAcc, wavesUsdPair)("WAVES") shouldBe bobWavesBalanceBefore - (sellOrderAmount + matcherFee)

      val aliceOrder   = matcherNode.prepareOrder(aliceAcc, wavesUsdPair, OrderType.BUY, price, buyOrderAmount)
      val aliceOrderId = matcherNode.placeOrder(aliceOrder).message.id
      matcherNode.waitOrderStatus(wavesUsdPair, aliceOrderId, "Filled", 1.minute)

      val aliceOrder2   = matcherNode.prepareOrder(aliceAcc, wavesUsdPair, OrderType.BUY, price, buyOrderAmount)
      val aliceOrder2Id = matcherNode.placeOrder(aliceOrder2).message.id
      matcherNode.waitOrderStatus(wavesUsdPair, aliceOrder2Id, "Filled", 1.minute)

      // Bob wants to buy some USD
      matcherNode.waitOrderStatus(wavesUsdPair, bobOrderId, "Filled", 1.minute)

      // Each side get fair amount of assets
      val exchangeTx = matcherNode.transactionsByOrder(bobOrder.idStr()).headOption.getOrElse(fail("Expected an exchange transaction"))
      nodes.waitForHeightAriseAndTxPresent(exchangeTx.id)
      matcherNode.reservedBalance(bobAcc) shouldBe empty
      matcherNode.reservedBalance(aliceAcc) shouldBe empty
    }
  }

  def correctAmount(a: Long, price: Long): Long = {
    val settledTotal = (BigDecimal(price) * a / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
    (BigDecimal(settledTotal) / price * Order.PriceConstant).setScale(0, RoundingMode.CEILING).toLong
  }

  def receiveAmount(ot: OrderType, matchPrice: Long, matchAmount: Long): Long =
    if (ot == BUY) correctAmount(matchAmount, matchPrice)
    else {
      (BigInt(matchAmount) * matchPrice / Order.PriceConstant).bigInteger.longValueExact()
    }

}

package com.wavesplatform.it.sync

import akka.http.scaladsl.model.StatusCodes._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
import com.wavesplatform.transaction.assets.exchange.OrderType.BUY

class RatesTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = {

    val orderFeeSettingsStr =
      s"""
         |waves.matcher {
         |  allowed-order-versions = [1, 2, 3]
         |  order-fee {
         |    mode = dynamic
         |    dynamic {
         |      base-fee = 300000
         |    }
         |  }  
         |}
       """.stripMargin

    super.nodeConfigs.map(
      ConfigFactory
        .parseString(orderFeeSettingsStr)
        .withFallback
    )
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Seq(IssueUsdTx, IssueWctTx, IssueBtcTx).map(_.json()).map(node.broadcastRequest(_)).foreach(tx => node.waitForTransaction(tx.id))
  }

  val defaultRateMap: Map[Asset, Double] = Map(Waves -> 1d)

  val wctRate        = 0.2
  val wctRateUpdated = 0.5

  val wctStr   = WctId.base58
  val wctAsset = IssuedAsset(WctId)

  val btcStr   = BtcId.base58
  val btcAsset = IssuedAsset(BtcId)

  val (amount, price) = (1000L, PriceConstant)

  def getOrder: Order = node.prepareOrder(alice, wctUsdPair, BUY, amount, price, fee = matcherFee, version = 3, matcherFeeAssetId = btcAsset)

  "Rates can be handled via REST" in {
    // default rates
    node.getRates shouldBe defaultRateMap

    // add rate for wct
    node.upsertRate(wctAsset, wctRate, expectedStatusCode = Created).message shouldBe s"Rate $wctRate for the asset $wctStr added"
    node.getRates shouldBe defaultRateMap + (wctAsset -> wctRate)

    // update rate for wct
    node
      .upsertRate(wctAsset, wctRateUpdated, expectedStatusCode = OK)
      .message shouldBe s"Rate for the asset $wctStr updated, old value = $wctRate, new value = $wctRateUpdated"
    node.getRates shouldBe defaultRateMap + (wctAsset -> wctRateUpdated)

    // update rate for Waves is not allowed
    node.upsertRate(Waves, wctRateUpdated, expectedStatusCode = BadRequest).message shouldBe "Rate for Waves cannot be changed"
    node.getRates shouldBe defaultRateMap + (wctAsset -> wctRateUpdated)

    // delete rate for wct
    node.deleteRate(wctAsset).message shouldBe s"Rate for the asset $wctStr deleted, old value = $wctRateUpdated"
    node.getRates shouldBe defaultRateMap
  }

  "Changing rates affects order validation" in {
    // set rate for btc
    node.upsertRate(btcAsset, 1, expectedStatusCode = Created)

    // place order with admissible fee (according to btc rate = 1)
    val placedOrderId1 = node.placeOrder(getOrder).message.id
    node.waitOrderStatus(wctUsdPair, placedOrderId1, "Accepted")

    // slightly increase rate for btc
    node.upsertRate(btcAsset, 1.1, expectedStatusCode = OK)

    // the same order now is rejected
    node.expectIncorrectOrderPlacement(
      getOrder,
      400,
      "OrderRejected",
      Some(s"Required ${(matcherFee * 1.1).toLong} $btcStr as fee for this order, but given $matcherFee $btcStr")
    )

    // return previous rate for btc
    node.upsertRate(btcAsset, 1, expectedStatusCode = OK)

    val placedOrderId2 = node.placeOrder(getOrder).message.id
    node.waitOrderStatus(wctUsdPair, placedOrderId2, "Accepted")

    node.deleteRate(btcAsset)
  }

  "Rates are restored from the DB after matcher's restart" in {
    // add high rate for btc
    node.upsertRate(btcAsset, 1.1, expectedStatusCode = Created)

    // order with low fee should be rejected
    node.expectIncorrectOrderPlacement(
      getOrder,
      400,
      "OrderRejected",
      Some(s"Required ${(matcherFee * 1.1).toLong} $btcStr as fee for this order, but given $matcherFee $btcStr")
    )

    // restart matcher
    docker.restartNode(node)

    // order with low fee should be rejected again
    node.expectIncorrectOrderPlacement(
      getOrder,
      400,
      "OrderRejected",
      Some(s"Required ${(matcherFee * 1.1).toLong} $btcStr as fee for this order, but given $matcherFee $btcStr")
    )
  }
}

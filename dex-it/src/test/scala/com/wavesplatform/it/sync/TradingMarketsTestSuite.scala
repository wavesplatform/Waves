package com.wavesplatform.it.sync

import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.assets.exchange.OrderType.BUY

class TradingMarketsTestSuite extends MatcherSuiteBase {
  val (amount, price) = (1000L, 1000000000L)

  "When some orders were placed and matcher was restarted" - {
    "Trading markets have info about all asset pairs" in {
      val wctTxId = node.broadcastRequest(IssueWctTx.json()).id
      node.waitForTransaction(wctTxId)
      node.waitForHeight(node.height + 1)

      val order = node.placeOrder(alice, wctWavesPair, BUY, amount, price, matcherFee).message.id
      node.waitOrderStatus(wctWavesPair, order, "Accepted")

      docker.restartNode(node)

      val markets = node.tradingMarkets().markets
      markets.size shouldBe 1
      markets.head.amountAssetName shouldNot be("Unknown")
      markets.head.priceAssetName shouldNot be("Unknown")
    }
  }
}

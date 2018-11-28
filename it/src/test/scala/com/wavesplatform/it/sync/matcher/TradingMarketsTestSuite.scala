package com.wavesplatform.it.sync.matcher

import com.typesafe.config.Config
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.transaction.assets.exchange.OrderType.BUY
import org.scalatest._

class TradingMarketsTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with NodesFromDocker
    with ReportingTestName {
  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcher = dockerNodes().head
  private def alice   = dockerNodes()(1)

  val (amount, price) = (1000L, 1000000000L)

  val wctTxId = matcher.signedIssue(createSignedIssueRequest(IssueWctTx)).id
  matcher.waitForTransaction(wctTxId)

  "When some orders were placed and matcher was restarted" - {
    val order = matcher.placeOrder(alice.privateKey, wctWavesPair, BUY, amount, price, matcherFee).message.id
    matcher.waitOrderStatus(wctWavesPair, order, "Accepted")

    docker.restartNode(matcher)

    "Trading markets have info about all asset pairs" in {
      val markets = matcher.tradingMarkets().markets
      markets.size shouldBe 1
      markets.head.amountAssetName shouldNot be("Unknown")
      markets.head.priceAssetName shouldNot be("Unknown")
    }
  }
}

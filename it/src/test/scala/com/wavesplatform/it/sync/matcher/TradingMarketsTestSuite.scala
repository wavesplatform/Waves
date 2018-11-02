package com.wavesplatform.it.sync.matcher

import com.typesafe.config.Config
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
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
    with ReportingTestName
    with GivenWhenThen {
  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcher = dockerNodes().head
  private def alice   = dockerNodes()(1)
  private def bob     = dockerNodes()(2)

  Seq(IssueUsdTx, IssueWctTx, IssueEthTx, IssueBtcTx).map(createSignedIssueRequest).map(matcher.signedIssue).foreach { tx =>
    matcher.waitForTransaction(tx.id)
  }

  val (dec2, dec8) = (1000L, 1000000000L)

  "When some orders was placed and matcher was restarted" - {
    val usdOrder = matcher.placeOrder(alice, wavesUsdPair, BUY, dec8, dec2).message.id
    val wctOrder = matcher.placeOrder(alice, wctWavesPair, BUY, dec2, dec8).message.id

    matcher.waitOrderStatus(wctWavesPair, wctOrder, "Accepted")

    docker.restartNode(matcher)

    "Trading markets have info about all asset pairs" in {

      val markets = matcher.tradingMarkets().markets
      markets.size shouldBe 2
      markets.foreach(_.amountAssetName shouldNot be("Unknown"))
      markets.foreach(_.priceAssetName shouldNot be("Unknown"))
    }

  }
}

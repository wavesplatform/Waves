package com.wavesplatform.it.sync.matcher.features

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory.parseString
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.assets.exchange.OrderType._
import com.wavesplatform.transaction.smart.script.v1.ScriptV1
import org.scalatest._

class SmartAssetsAndNoAccTradingTestSuite extends MatcherSuiteBase with GivenWhenThen {

  import SmartAssetsAndNoAccTradingTestSuite._
  override protected def nodeConfigs: Seq[Config] = Configs.map(configWithPreActivatedFeatures().withFallback(_))

  val (amount, price) = (1000L, 1000000000L)

  "SmartAssets feature should work when SmartAccTrading is still not activated" in {
    val asset = bobNode
      .issue(
        bobAcc.address,
        "Asset",
        "For test",
        defaultAssetQuantity,
        0,
        reissuable = false,
        issueFee,
        waitForTx = true,
        script = Some(ScriptV1(Terms.TRUE).explicitGet().bytes().base64)
      )
      .id
    val pair = AssetPair(ByteStr.decodeBase58(asset).toOption, None)

    val buyOrder = matcherNode.placeOrder(aliceAcc, pair, BUY, amount, price, smartTradeFee, version = 1)
    matcherNode.waitOrderStatus(pair, buyOrder.message.id, "Accepted")

    val sellOrder = matcherNode.placeOrder(bobAcc, pair, SELL, amount, price, smartTradeFee, version = 1)
    matcherNode.waitOrderStatus(pair, sellOrder.message.id, "Filled")

    matcherNode.waitOrderInBlockchain(sellOrder.message.id)
  }

}

object SmartAssetsAndNoAccTradingTestSuite {
  def configWithPreActivatedFeatures(): Config = {
    parseString(s"""
                   |waves {
                   |  blockchain.custom.functionality {
                   |    pre-activated-features = { ${BlockchainFeatures.SmartAssets.id} = 0, ${BlockchainFeatures.SmartAccountTrading.id} = 50 }
                   |  }
                   |  miner.quorum = 1
                   |}
    """.stripMargin)
  }
}

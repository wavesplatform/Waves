package com.wavesplatform.it.sync.matcher.features

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory.parseString
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.assets.exchange.OrderType._
import org.scalatest._

class AccTradingAndNoSmartAssetsTestSuite extends MatcherSuiteBase with GivenWhenThen {

  import AccTradingAndNoSmartAssetsTestSuite._
  override protected def nodeConfigs: Seq[Config] = Configs.map(configWithPreActivatedFeatures().withFallback(_))

  val (amount, price) = (1000L, 1000000000L)

  "SmartAccTrading feature should work when SmartAssets is still not activated" in {
    setContract(Some("true"), bobAcc)

    val asset = bobNode
      .issue(
        bobAcc.address,
        "Asset",
        "For test",
        defaultAssetQuantity,
        0,
        reissuable = false,
        smartIssueFee,
        waitForTx = true,
        version = 2
      )
      .id
    val pair = AssetPair(ByteStr.decodeBase58(asset).toOption, None)

    val buyOrder = matcherNode.placeOrder(aliceAcc, pair, BUY, amount, price, tradeFee, version = 1)
    matcherNode.waitOrderStatus(pair, buyOrder.message.id, "Accepted")

    val sellOrder = matcherNode.placeOrder(bobAcc, pair, SELL, amount, price, tradeFee, version = 2)
    matcherNode.waitOrderStatus(pair, sellOrder.message.id, "Filled")

    matcherNode.waitOrderInBlockchain(sellOrder.message.id)
  }

}

object AccTradingAndNoSmartAssetsTestSuite {
  def configWithPreActivatedFeatures(): Config = {
    parseString(s"""
                   |waves {
                   |  blockchain.custom.functionality {
                   |    pre-activated-features = { ${BlockchainFeatures.SmartAssets.id} = 50, ${BlockchainFeatures.SmartAccountTrading.id} = 0 }
                   |  }
                   |  miner.quorum = 1
                   |}
    """.stripMargin)
  }
}

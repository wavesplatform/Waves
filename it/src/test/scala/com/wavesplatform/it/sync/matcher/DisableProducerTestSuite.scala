package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.api.SyncHttpApi.{sync => _, _}
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.matcher.config.MatcherDefaultConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.duration._
import scala.util.Random

class DisableProducerTestSuite extends MatcherSuiteBase {
  private def matcherConfig                       = ConfigFactory.parseString(s"""waves.matcher.events-queue {
       |  local.enable-storing  = no
       |  kafka.producer.enable = no
       |}""".stripMargin)
  override protected def nodeConfigs: Seq[Config] = Configs.map(matcherConfig.withFallback)
  private def orderVersion                        = (Random.nextInt(2) + 1).toByte

  "check no events are written to queue" - {
    // Alice issues new asset
    val aliceAsset =
      aliceNode
        .issue(aliceAcc.address, "DisconnectCoin", "Alice's coin for disconnect tests", someAssetAmount, 0, reissuable = false, smartIssueFee, 2)
        .id
    matcherNode.waitForTransaction(aliceAsset)

    val aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)
    // check assets's balances
    matcherNode.assertAssetBalance(aliceAcc.address, aliceAsset, someAssetAmount)
    matcherNode.assertAssetBalance(matcherAcc.address, aliceAsset, 0)

    "place an order and wait some time" in {
      // Alice places sell order
      val order1 =
        matcherNode.prepareOrder(aliceAcc, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, matcherFee, orderVersion, 20.days)

      matcherNode
        .expectIncorrectOrderPlacement(
          order1,
          expectedStatusCode = 501,
          expectedStatus = "Disabled",
          expectedMessage = Some("This functionality is disabled, contact with admins")
        )

      // Alice places buy order
      val order2 =
        matcherNode.prepareOrder(aliceAcc, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, matcherFee, orderVersion, 21.days)

      matcherNode
        .expectIncorrectOrderPlacement(
          order2,
          expectedStatusCode = 501,
          expectedStatus = "Disabled",
          expectedMessage = Some("This functionality is disabled, contact with admins")
        )

      Thread.sleep(5000)
      matcherNode.getCurrentOffset should be(-1)
      matcherNode.getLastOffset should be(-1)

      docker.killAndStartContainer(dockerNodes().head)

      matcherNode.getCurrentOffset should be(-1)
      matcherNode.getLastOffset should be(-1)
    }
  }
}

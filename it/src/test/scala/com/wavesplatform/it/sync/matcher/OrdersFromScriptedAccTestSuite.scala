package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.util._
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.wavesplatform.it.sync._
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import play.api.libs.json.JsNumber

import scala.concurrent.duration._

class OrdersFromScriptedAccTestSuite extends MatcherSuiteBase {

  import OrdersFromScriptedAccTestSuite._

  override protected def nodeConfigs: Seq[Config] = updatedConfigs

  "issue asset and run test" - {
    // Alice issues new asset
    val aliceAsset =
      aliceNode.issue(aliceAcc.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, issueFee, 2).id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)
    val aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    "setScript at account" in {
      // check assets's balances
      aliceNode.assertAssetBalance(aliceAcc.address, aliceAsset, someAssetAmount)
      matcherNode.assertAssetBalance(matcherAcc.address, aliceAsset, 0)

      val scriptText = s"""true""".stripMargin

      val script = ScriptCompiler(scriptText).explicitGet()._1
      val setScriptTransaction = SetScriptTransaction
        .selfSigned(SetScriptTransaction.supportedVersions.head, bobAcc, Some(script), setScriptFee, System.currentTimeMillis())
        .right
        .get

      val setScriptId = bobNode
        .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
        .id

      nodes.waitForHeightAriseAndTxPresent(setScriptId)
    }

    "trading is deprecated" in {
      assertBadRequestAndResponse(
        matcherNode
          .placeOrder(bobAcc, aliceWavesPair, OrderType.BUY, 2.waves * Order.PriceConstant, 500, version = 1, 10.minutes),
        "Trading on scripted account isn't allowed yet."
      )
    }

    "scripted account can trade once SmartAccountTrading is activated" in {
      matcherNode.waitForHeight(ActivationHeight, 3.minutes)
      val bobOrder = matcherNode
        .placeOrder(bobAcc, aliceWavesPair, OrderType.BUY, 2.waves * Order.PriceConstant, 500, version = 1, 10.minutes)
      bobOrder.status shouldBe "OrderAccepted"
    }

    "can trade from non-scripted account" in {
      // Alice places sell order
      val aliceOrder = matcherNode
        .placeOrder(aliceAcc, aliceWavesPair, OrderType.SELL, 2.waves * Order.PriceConstant, 500, version = 1, 10.minutes)

      aliceOrder.status shouldBe "OrderAccepted"

      val orderId = aliceOrder.message.id

      // Alice checks that the order in order book
      matcherNode.waitOrderStatus(aliceWavesPair, orderId, "Filled")
      matcherNode.fullOrderHistory(aliceAcc).head.status shouldBe "Filled"

    }
  }
}

object OrdersFromScriptedAccTestSuite {
  val ActivationHeight = 25

  import com.wavesplatform.it.sync.matcher.config.MatcherDefaultConfig._

  private val matcherConfig = ConfigFactory.parseString(s"""
                                                           |waves {
                                                           |  blockchain.custom.functionality.pre-activated-features = { 10 = $ActivationHeight }
                                                           |}""".stripMargin)

  private val updatedConfigs: Seq[Config] = Configs.map(matcherConfig.withFallback(_))
}

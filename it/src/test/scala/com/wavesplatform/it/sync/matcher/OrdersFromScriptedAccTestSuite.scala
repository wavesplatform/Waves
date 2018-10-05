package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it._
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}
import com.wavesplatform.it.sync._
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import play.api.libs.json.JsNumber

import scala.concurrent.duration._
import scala.util.Random

class OrdersFromScriptedAccTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with ReportingTestName
    with NodesFromDocker {

  import OrdersFromScriptedAccTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)
  private def bobNode   = nodes(2)

  "issue asset and run test" - {
    // Alice issues new asset
    val aliceAsset =
      aliceNode.issue(aliceNode.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, 100000000L).id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)
    val aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    // check assets's balances
    aliceNode.assertAssetBalance(aliceNode.address, aliceAsset, someAssetAmount)
    aliceNode.assertAssetBalance(matcherNode.address, aliceAsset, 0)

    "setScript at account" in {
      val scriptText = s"""true""".stripMargin

      val script = ScriptCompiler(scriptText).explicitGet()._1
      val setScriptTransaction = SetScriptTransaction
        .selfSigned(SetScriptTransaction.supportedVersions.head, bobNode.privateKey, Some(script), setScriptFee, System.currentTimeMillis())
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
          .placeOrder(bobNode, aliceWavesPair, OrderType.BUY, 2.waves * Order.PriceConstant, 500, version = 1, 10.minutes),
        "Trading on scripted account isn't allowed yet."
      )
    }

    "scripted account can trade once SmartAccountTrading is activated" in {
      matcherNode.waitForHeight(ActivationHeight, 3.minutes)
      val bobOrder = matcherNode
        .placeOrder(bobNode, aliceWavesPair, OrderType.BUY, 2.waves * Order.PriceConstant, 500, version = 1, 10.minutes)
      bobOrder.status shouldBe "OrderAccepted"
    }

    "can trade from non-scripted account" in {
      // Alice places sell order
      val aliceOrder = matcherNode
        .placeOrder(aliceNode, aliceWavesPair, OrderType.SELL, 2.waves * Order.PriceConstant, 500, version = 1, 10.minutes)

      aliceOrder.status shouldBe "OrderAccepted"

      val orderId = aliceOrder.message.id

      // Alice checks that the order in order book
      matcherNode.waitOrderStatus(aliceWavesPair, orderId, "Filled")
      matcherNode.fullOrderHistory(aliceNode).head.status shouldBe "Filled"

    }
  }
}

object OrdersFromScriptedAccTestSuite {
  val ActivationHeight = 15

  import NodeConfigs.Default

  private val matcherConfig = ConfigFactory.parseString(s"""
                                                           |waves {
                                                           |  matcher {
                                                           |    enable = yes
                                                           |    account = 3HmFkAoQRs4Y3PE2uR6ohN7wS4VqPBGKv7k
                                                           |    bind-address = "0.0.0.0"
                                                           |    order-match-tx-fee = 300000
                                                           |    order-cleanup-interval = 20s
                                                           |  }
                                                           |  rest-api {
                                                           |    enable = yes
                                                           |    api-key-hash = 7L6GpLHhA5KyJTAVc8WFHwEcyTY8fC8rRbyMCiFnM4i
                                                           |  }
                                                           |  miner.enable=no
                                                           |  blockchain.custom.functionality.pre-activated-features = { 10 = $ActivationHeight }
                                                           |
                                                           |}""".stripMargin)

  private val nonGeneratingPeersConfig = ConfigFactory.parseString(
    """waves {
      | matcher.order-cleanup-interval = 30s
      | miner.enable=no
      |}""".stripMargin
  )

  val MatcherFee: Long     = 300000
  val TransactionFee: Long = 300000

  private val Configs: Seq[Config] = {
    val notMatchingNodes = Random.shuffle(Default.init).take(3)
    Seq(matcherConfig.withFallback(Default.last), notMatchingNodes.head) ++
      notMatchingNodes.tail.map(nonGeneratingPeersConfig.withFallback)
  }
}

package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config}
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
import play.api.libs.json.JsNumber

import scala.concurrent.duration._
import com.wavesplatform.it.sync.matcher.config.MatcherDefaultConfig._
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class OrdersFromScriptedAccTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with ReportingTestName
    with NodesFromDocker {

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

    "make Bob's address as scripted" in {
      val scriptText = s"""true""".stripMargin

      val script = ScriptCompiler(scriptText).explicitGet()._1
      val setScriptTransaction = SetScriptTransaction
        .selfSigned(SetScriptTransaction.supportedVersions.head, bobNode.privateKey, Some(script), minFee, System.currentTimeMillis())
        .right
        .get

      val setScriptId = bobNode
        .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
        .id

      nodes.waitForHeightAriseAndTxPresent(setScriptId)

    }

    "Alice place sell order, but Bob cannot place order, because his acc is scripted" in {
      // Alice places sell order
      val aliceOrder = matcherNode
        .placeOrder(aliceNode, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, 10.minutes)

      aliceOrder.status shouldBe "OrderAccepted"

      val orderId = aliceOrder.message.id

      // Alice checks that the order in order book
      matcherNode.waitOrderStatus(aliceWavesPair, orderId, "Accepted")
      matcherNode.fullOrderHistory(aliceNode).head.status shouldBe "Accepted"

      // Alice check that order is correct
      val orders = matcherNode.orderBook(aliceWavesPair)
      orders.asks.head.amount shouldBe 500
      orders.asks.head.price shouldBe 2.waves * Order.PriceConstant

      // sell order should be in the aliceNode orderbook
      matcherNode.fullOrderHistory(aliceNode).head.status shouldBe "Accepted"

      // Bob gets error message
      assertBadRequestAndResponse(
        matcherNode
          .placeOrder(bobNode, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, 10.minutes),
        "Trading on scripted account isn't allowed yet."
      )

    }
  }

}

package com.wavesplatform.it.sync.matcher.smartcontracts

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.state.{ByteStr, EitherExt2}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import scala.concurrent.duration._

class OrdersFromScriptedAccTestSuite extends MatcherSuiteBase {

  import OrdersFromScriptedAccTestSuite._

  override protected def nodeConfigs: Seq[Config] = updatedConfigs

  private val sDupNames =
    """let x = (let x = 2
      |3)
      |x == 3""".stripMargin

  "issue asset and run test" - {
    // Alice issues new asset
    val aliceAsset =
      aliceNode.issue(aliceAcc.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, issueFee, 2).id
    matcherNode.waitForTransaction(aliceAsset)
    val aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    "setScript at account" in {
      // check assets's balances
      matcherNode.assertAssetBalance(aliceAcc.address, aliceAsset, someAssetAmount)
      matcherNode.assertAssetBalance(matcherAcc.address, aliceAsset, 0)

      withClue("mining was too fast, can't continue") {
        matcherNode.height shouldBe <(ActivationHeight)
      }

      withClue("duplicate names in contracts are denied") {
        val setScriptTransaction = SetScriptTransaction
          .selfSigned(
            SetScriptTransaction.supportedVersions.head,
            bobAcc,
            Some(ScriptCompiler(sDupNames, isAssetScript = false).explicitGet()._1),
            0.014.waves,
            System.currentTimeMillis()
          )
          .explicitGet()

        assertBadRequestAndResponse(
          matcherNode
            .signedBroadcast(setScriptTransaction.json()),
          "VarNames: duplicate variable names are temporarily denied:"
        )
      }

      setContract(Some("true"), bobAcc)
    }

    "trading is deprecated" in {
      assertBadRequestAndResponse(
        matcherNode.placeOrder(bobAcc, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 1, 10.minutes),
        "Trading on scripted account isn't allowed yet"
      )
    }

    "can't place an OrderV2 before the activation" in {
      assertBadRequestAndResponse(
        matcherNode.placeOrder(bobAcc, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 2, 10.minutes),
        "Orders of version 1 are only accepted, because SmartAccountTrading has not been activated yet"
      )
    }

    "invalid setScript at account" in {
      matcherNode.waitForHeight(ActivationHeight, 5.minutes)
      setContract(Some("true && (height > 0)"), bobAcc)
      assertBadRequestAndResponse(
        matcherNode.placeOrder(bobAcc, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 2, 10.minutes),
        "height is inaccessible when running script on matcher"
      )
    }

    "scripted account can trade once SmartAccountTrading is activated" in {
      setContract(Some(sDupNames), bobAcc)
      val bobOrder =
        matcherNode.placeOrder(bobAcc, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 2, 10.minutes)
      bobOrder.status shouldBe "OrderAccepted"
    }

    "can trade from non-scripted account" in {
      // Alice places sell order
      val aliceOrder =
        matcherNode.placeOrder(aliceAcc, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, matcherFee, version = 1, 10.minutes)

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

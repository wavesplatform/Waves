package com.wavesplatform.it.sync.matcher.smartcontracts

import com.typesafe.config.Config
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class ExtraFeeTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = Configs

  val trueScript = Some(ScriptCompiler("true", isAssetScript = false).explicitGet()._1.bytes().base64) //TODO добавить типовые проверки в скрипт
  val amount     = 1L
  val price      = 100000000L

  // set smart account
  setContract(Some("true"), aliceAcc)

  // issue one simple and two smart assets
  val asset0: String = aliceNode
    .issue(aliceAcc.address, "Asset0", "Test", defaultAssetQuantity, 0, reissuable = false, smartIssueFee, 2)
    .id
  val asset1: String = aliceNode
    .issue(aliceAcc.address, "SmartAsset1", "Test", defaultAssetQuantity, 0, reissuable = false, smartIssueFee, 2, trueScript)
    .id
  val asset2: String = bobNode
    .issue(bobAcc.address, "SmartAsset2", "Test", defaultAssetQuantity, 0, reissuable = false, smartIssueFee, 2, trueScript)
    .id
  Seq(asset1, asset2).foreach(nodes.waitForTransaction(_))

  Seq(
    aliceNode.transfer(aliceAcc.address, bobAcc.address, defaultAssetQuantity / 2, 0.005.waves, Some(asset0), None, 2).id,
    aliceNode.transfer(aliceAcc.address, bobAcc.address, defaultAssetQuantity / 2, 0.009.waves, Some(asset1), None, 2).id,
    bobNode.transfer(bobAcc.address, aliceAcc.address, defaultAssetQuantity / 2, 0.005.waves, Some(asset2), None, 2).id
  ).foreach(matcherNode.waitForTransaction(_))

  "When matcher executes orders" - {
    "with one Smart Account and one Smart Asset" - {
      "then fee should be 0.003 + 0.004 (for Smart Asset only, not Smart Account)" in {
        val oneSmartPair = createAssetPair(asset0, asset1)

        val aliceInitBalance   = matcherNode.accountBalances(aliceAcc.address)._1
        val bobInitBalance     = matcherNode.accountBalances(bobAcc.address)._1
        val matcherInitBalance = matcherNode.accountBalances(matcherNode.address)._1

        val expectedFee = tradeFee + smartFee // 1 x "smart asset"
        val invalidFee  = expectedFee - 1

        matcherNode.expectRejectedOrderPlacement(aliceAcc,
                                                 oneSmartPair,
                                                 SELL,
                                                 amount,
                                                 price,
                                                 invalidFee,
                                                 2,
                                                 expectedMessage = Some("Required 700000 WAVES as fee for this order, but given 699999 WAVES"))

        val counter = matcherNode.placeOrder(aliceAcc, oneSmartPair, SELL, amount, price, expectedFee, 2).message.id
        matcherNode.waitOrderStatus(oneSmartPair, counter, "Accepted")

        info("expected fee should be reserved")
        matcherNode.reservedBalance(aliceAcc)("WAVES") shouldBe expectedFee

        val submitted = matcherNode.placeOrder(bobAcc, oneSmartPair, BUY, amount, price, expectedFee, 2).message.id
        matcherNode.waitOrderInBlockchain(submitted)

        matcherNode.accountBalances(aliceAcc.address)._1 shouldBe aliceInitBalance - expectedFee
        matcherNode.accountBalances(bobAcc.address)._1 shouldBe bobInitBalance - expectedFee
        matcherNode.accountBalances(matcherNode.address)._1 shouldBe matcherInitBalance + expectedFee
      }
    }

    "with one Smart Account, two Smart Assets and scripted Matcher" - {
      "then fee should be 0.003 + (0.004 * 2) + 0.004 (for Smart Assets and Matcher Script)" - {
        "and total fee should be divided proportionally with partial filling" in {
          setContract(Some("true"), matcherNode.privateKey)

          val bothSmartPair = createAssetPair(asset1, asset2)

          val aliceInitBalance   = matcherNode.accountBalances(aliceAcc.address)._1
          val bobInitBalance     = matcherNode.accountBalances(bobAcc.address)._1
          val matcherInitBalance = matcherNode.accountBalances(matcherNode.address)._1

          val expectedFee = tradeFee + 2 * smartFee + smartFee // 2 x "smart asset" and 1 x "matcher script"
          val invalidFee  = expectedFee - 1

          matcherNode.expectRejectedOrderPlacement(
            aliceAcc,
            bothSmartPair,
            SELL,
            amount,
            price,
            invalidFee,
            2,
            expectedMessage = Some("Required 1500000 WAVES as fee for this order, but given 1499999 WAVES")
          )

          val counter = matcherNode.placeOrder(aliceAcc, bothSmartPair, SELL, amount, price, expectedFee, 2).message.id
          matcherNode.waitOrderStatus(bothSmartPair, counter, "Accepted")

          info("expected fee should be reserved")
          matcherNode.reservedBalance(aliceAcc)("WAVES") shouldBe expectedFee

          val submitted = matcherNode.placeOrder(bobAcc, bothSmartPair, BUY, amount, price, expectedFee, 2).message.id
          matcherNode.waitOrderInBlockchain(submitted)

          matcherNode.accountBalances(aliceAcc.address)._1 shouldBe aliceInitBalance - expectedFee
          matcherNode.accountBalances(bobAcc.address)._1 shouldBe bobInitBalance - expectedFee
          matcherNode.accountBalances(matcherNode.address)._1 shouldBe matcherInitBalance + expectedFee
        }
      }
    }
  }

}

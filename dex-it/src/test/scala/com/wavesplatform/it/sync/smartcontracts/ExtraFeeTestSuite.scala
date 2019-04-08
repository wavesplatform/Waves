package com.wavesplatform.it.sync.smartcontracts

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util.DoubleExt
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class ExtraFeeTestSuite extends MatcherSuiteBase {
  val trueScript = Some(ScriptCompiler("true", isAssetScript = false).explicitGet()._1.bytes().base64) //TODO добавить типовые проверки в скрипт
  val amount     = 1L
  val price      = 100000000L

  // set smart account
  setContract(Some("true"), alice)

  // issue one simple and two smart assets
  val asset0: String = node
    .broadcastIssue(alice, "Asset0", "Test", defaultAssetQuantity, 0, reissuable = false, smartIssueFee, None)
    .id
  val asset1: String = node
    .broadcastIssue(alice, "SmartAsset1", "Test", defaultAssetQuantity, 0, reissuable = false, smartIssueFee, trueScript)
    .id
  val asset2: String = node
    .broadcastIssue(bob, "SmartAsset2", "Test", defaultAssetQuantity, 0, reissuable = false, smartIssueFee, trueScript)
    .id
  Seq(asset0, asset1, asset2).foreach(node.waitForTransaction(_))

  // distribute
  {
    val xs = Seq(
      node.broadcastTransfer(alice, bob.address, defaultAssetQuantity / 2, 0.005.waves, Some(asset0), None).id,
      node.broadcastTransfer(alice, bob.address, defaultAssetQuantity / 2, 0.009.waves, Some(asset1), None).id,
      node.broadcastTransfer(bob, alice.address, defaultAssetQuantity / 2, 0.005.waves, Some(asset2), None).id
    )
    xs.foreach(node.waitForTransaction(_))
  }

  "When matcher executes orders" - {
    "with one Smart Account and one Smart Asset" - {
      "then fee should be 0.003 + 0.004 (for Smart Asset only, not Smart Account)" in {
        val oneSmartPair = createAssetPair(asset0, asset1)

        val aliceInitBalance   = node.accountBalances(alice.address)._1
        val bobInitBalance     = node.accountBalances(bob.address)._1
        val matcherInitBalance = node.accountBalances(matcher.address)._1

        val expectedFee = tradeFee + smartFee // 1 x "smart asset"
        val invalidFee  = expectedFee - 1

        node.expectRejectedOrderPlacement(
          alice,
          oneSmartPair,
          SELL,
          amount,
          price,
          invalidFee,
          2,
          expectedMessage = Some("Required 700000 WAVES as fee for this order, but given 699999 WAVES")
        )

        val counter = node.placeOrder(alice, oneSmartPair, SELL, amount, price, expectedFee, 2).message.id
        node.waitOrderStatus(oneSmartPair, counter, "Accepted")

        info("expected fee should be reserved")
        node.reservedBalance(alice)("WAVES") shouldBe expectedFee

        val submitted = node.placeOrder(bob, oneSmartPair, BUY, amount, price, expectedFee, 2).message.id
        node.waitOrderInBlockchain(submitted)

        node.accountBalances(alice.address)._1 shouldBe aliceInitBalance - expectedFee
        node.accountBalances(bob.address)._1 shouldBe bobInitBalance - expectedFee
        node.accountBalances(matcher.address)._1 shouldBe matcherInitBalance + expectedFee
      }
    }

    "with one Smart Account, two Smart Assets and scripted Matcher" - {
      "then fee should be 0.003 + (0.004 * 2) + 0.004 (for Smart Assets and Matcher Script)" - {
        "and total fee should be divided proportionally with partial filling" in {
          setContract(Some("true"), matcher)

          val bothSmartPair = createAssetPair(asset1, asset2)

          val aliceInitBalance   = node.accountBalances(alice.address)._1
          val bobInitBalance     = node.accountBalances(bob.address)._1
          val matcherInitBalance = node.accountBalances(matcher.address)._1

          val expectedFee = tradeFee + 2 * smartFee + smartFee // 2 x "smart asset" and 1 x "matcher script"
          val invalidFee  = expectedFee - 1

          node.expectRejectedOrderPlacement(
            alice,
            bothSmartPair,
            SELL,
            amount,
            price,
            invalidFee,
            2,
            expectedMessage = Some("Required 1500000 WAVES as fee for this order, but given 1499999 WAVES")
          )

          val counter = node.placeOrder(alice, bothSmartPair, SELL, amount, price, expectedFee, 2).message.id
          node.waitOrderStatus(bothSmartPair, counter, "Accepted")

          info("expected fee should be reserved")
          node.reservedBalance(alice)("WAVES") shouldBe expectedFee

          val submitted = node.placeOrder(bob, bothSmartPair, BUY, amount, price, expectedFee, 2).message.id
          node.waitOrderInBlockchain(submitted)

          node.accountBalances(alice.address)._1 shouldBe aliceInitBalance - expectedFee
          node.accountBalances(bob.address)._1 shouldBe bobInitBalance - expectedFee
          node.accountBalances(matcher.address)._1 shouldBe matcherInitBalance + expectedFee
        }
      }
    }
  }

}

package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{setScriptFee, smartFee, smartMinFee}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class InvokeScriptPayAndTransferSameAssetSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val estimator = ScriptEstimatorV2

  private val dApp     = pkByAddress(firstAddress).stringRepr
  private val caller   = pkByAddress(secondAddress).stringRepr
  private val receiver = pkByAddress(thirdAddress).stringRepr

  var dAppInitBalance: Long     = 0
  var callerInitBalance: Long   = 0
  var receiverInitBalance: Long = 0
  val assetQuantity: Long       = 15
  var assetId: String           = ""
  var smartAssetId: String      = ""
  var rejAssetId: String        = ""

  test("_issue and transfer asset") {
    assetId = sender.issue(caller, "Asset", "a", assetQuantity, 0).id

    val script = Some(ScriptCompiler.compile("true", estimator).explicitGet()._1.bytes.value.base64)
    smartAssetId = sender.issue(caller, "Smart", "s", assetQuantity, 0, script = script).id

    val scriptText  = "match tx {case t:TransferTransaction => false case _ => true}"
    val smartScript = Some(ScriptCompiler.compile(scriptText, estimator).explicitGet()._1.bytes.value.base64)
    rejAssetId = sender.issue(caller, "Reject", "r", assetQuantity, 0, script = smartScript, waitForTx = true).id
  }

  test("_set script to dApp account and transfer out all waves") {
    val dAppBalance = sender.accountBalances(dApp)._1
    sender.transfer(dApp, caller, dAppBalance - smartMinFee - setScriptFee, smartMinFee, waitForTx = true).id

    val dAppScript        = ScriptCompiler.compile(s"""
          |{-# STDLIB_VERSION 3 #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |let receiver = Address(base58'$receiver')
          |
          |@Callable(i)
          |func resendPayment() = {
          |  if (isDefined(i.payment)) then
          |    let pay = extract(i.payment)
          |    TransferSet([ScriptTransfer(receiver, 1, pay.assetId)])
          |  else throw("need payment in WAVES or any Asset")
          |}
        """.stripMargin, estimator).explicitGet()._1
    sender.setScript(dApp, Some(dAppScript.bytes().base64), waitForTx = true).id

  }

  test("dApp can transfer payed asset if its own balance is 0") {
    dAppInitBalance = sender.accountBalances(dApp)._1
    callerInitBalance = sender.accountBalances(caller)._1
    receiverInitBalance = sender.accountBalances(receiver)._1

    val paymentAmount = 10

    invoke("resendPayment", paymentAmount, issued(assetId))

    sender.accountBalances(dApp)._1 shouldBe dAppInitBalance
    sender.accountBalances(caller)._1 shouldBe callerInitBalance - smartMinFee
    sender.accountBalances(receiver)._1 shouldBe receiverInitBalance

    sender.assetBalance(dApp, assetId).balance shouldBe paymentAmount - 1
    sender.assetBalance(caller, assetId).balance shouldBe assetQuantity - paymentAmount
    sender.assetBalance(receiver, assetId).balance shouldBe 1
  }

  test("dApp can transfer payed smart asset if its own balance is 0") {
    dAppInitBalance = sender.accountBalances(dApp)._1
    callerInitBalance = sender.accountBalances(caller)._1
    receiverInitBalance = sender.accountBalances(receiver)._1

    val paymentAmount = 10
    val fee           = smartMinFee + smartFee * 2

    invoke("resendPayment", paymentAmount, issued(smartAssetId), fee)

    sender.accountBalances(dApp)._1 shouldBe dAppInitBalance
    sender.accountBalances(caller)._1 shouldBe callerInitBalance - fee
    sender.accountBalances(receiver)._1 shouldBe receiverInitBalance

    sender.assetBalance(dApp, smartAssetId).balance shouldBe paymentAmount - 1
    sender.assetBalance(caller, smartAssetId).balance shouldBe assetQuantity - paymentAmount
    sender.assetBalance(receiver, smartAssetId).balance shouldBe 1
  }

  test("dApp can't transfer payed smart asset if it rejects transfers and its own balance is 0") {
    dAppInitBalance = sender.accountBalances(dApp)._1
    callerInitBalance = sender.accountBalances(caller)._1
    receiverInitBalance = sender.accountBalances(receiver)._1

    val paymentAmount = 10
    val fee           = smartMinFee + smartFee * 2

    assertBadRequestAndMessage(
      invoke("resendPayment", paymentAmount, issued(rejAssetId), fee),
      "token-script"
    )
  }

  test("dApp can transfer payed Waves if its own balance is 0") {
    dAppInitBalance = sender.accountBalances(dApp)._1
    callerInitBalance = sender.accountBalances(caller)._1
    receiverInitBalance = sender.accountBalances(receiver)._1

    dAppInitBalance shouldBe 0

    val paymentAmount    = 10
    invoke("resendPayment", paymentAmount)

    sender.accountBalances(dApp)._1 shouldBe dAppInitBalance + paymentAmount - 1
    sender.accountBalances(caller)._1 shouldBe callerInitBalance - paymentAmount - smartMinFee
    sender.accountBalances(receiver)._1 shouldBe receiverInitBalance + 1
  }

  def issued(assetId: String): Asset = IssuedAsset(ByteStr.decodeBase58(assetId).get)

  def invoke(func: String, amount: Long, asset: Asset = Waves, fee: Long = 500000): String = {
    sender
      .invokeScript(
        caller,
        dApp,
        Some(func),
        payment = Seq(Payment(amount, asset)),
        fee = fee,
        waitForTx = true
      )
      ._1.id
  }

}

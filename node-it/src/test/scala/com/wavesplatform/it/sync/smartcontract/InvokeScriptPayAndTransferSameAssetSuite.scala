package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.api.http.ApiError.TransactionNotAllowedByAssetScript
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{setScriptFee, smartFee, smartMinFee}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class InvokeScriptPayAndTransferSameAssetSuite extends BaseTransactionSuite {
  private val estimator = ScriptEstimatorV2

  private def dApp = firstKeyPair

  private def caller = secondKeyPair

  private def receiver = thirdKeyPair

  private lazy val dAppAddress: String = dApp.toAddress.toString
  private lazy val callerAddress: String = caller.toAddress.toString
  private lazy val receiverAddress: String = receiver.toAddress.toString

  var dAppInitBalance: Long     = 0
  var callerInitBalance: Long   = 0
  var receiverInitBalance: Long = 0
  val assetQuantity: Long       = 15
  var assetId: String           = ""
  var smartAssetId: String      = ""
  var rejAssetId: String        = ""

  test("_issue and transfer asset") {
    assetId = miner.issue(caller, "Asset", "a", assetQuantity, 0).id

    val script = Some(ScriptCompiler.compile("true", estimator).explicitGet()._1.bytes().base64)
    smartAssetId = miner.issue(caller, "Smart", "s", assetQuantity, 0, script = script).id

    val scriptText  = "match tx {case _:TransferTransaction => false case _ => true}"
    val smartScript = Some(ScriptCompiler.compile(scriptText, estimator).explicitGet()._1.bytes().base64)
    rejAssetId = miner.issue(caller, "Reject", "r", assetQuantity, 0, script = smartScript, waitForTx = true).id
  }

  test("_set script to dApp account and transfer out all waves") {
    val dAppBalance = miner.wavesBalance(dAppAddress)
    miner.transfer(dApp, callerAddress, dAppBalance - smartMinFee - setScriptFee, smartMinFee, waitForTx = true).id

    val dAppScript = ScriptCompiler
      .compile(
        s"""
          |{-# STDLIB_VERSION 3 #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |let receiver = Address(base58'$receiverAddress')
          |
          |@Callable(i)
          |func resendPayment() = {
          |  if (isDefined(i.payment)) then
          |    let pay = extract(i.payment)
          |    TransferSet([ScriptTransfer(receiver, 1, pay.assetId)])
          |  else throw("need payment in WAVES or any Asset")
          |}
        """.stripMargin,
        estimator
      )
      .explicitGet()
      ._1
    miner.setScript(dApp, Some(dAppScript.bytes().base64), waitForTx = true).id

  }

  test("dApp can transfer payed asset if its own balance is 0") {
    dAppInitBalance = miner.wavesBalance(dAppAddress)
    callerInitBalance = miner.wavesBalance(callerAddress)
    receiverInitBalance = miner.wavesBalance(receiverAddress)
    val paymentAmount = 10

    invoke("resendPayment", paymentAmount, issued(assetId))

    miner.wavesBalance(dAppAddress) shouldBe dAppInitBalance
    miner.wavesBalance(callerAddress) shouldBe callerInitBalance - smartMinFee
    miner.wavesBalance(receiverAddress) shouldBe receiverInitBalance

    miner.assetBalance(dAppAddress, assetId).balance shouldBe paymentAmount - 1
    miner.assetBalance(callerAddress, assetId).balance shouldBe assetQuantity - paymentAmount
    miner.assetBalance(receiverAddress, assetId).balance shouldBe 1
  }

  test("dApp can transfer payed smart asset if its own balance is 0") {
    dAppInitBalance = miner.wavesBalance(dAppAddress)
    callerInitBalance = miner.wavesBalance(callerAddress)
    receiverInitBalance = miner.wavesBalance(receiverAddress)
    val paymentAmount = 10
    val fee           = smartMinFee + smartFee * 2

    invoke("resendPayment", paymentAmount, issued(smartAssetId), fee)

    miner.wavesBalance(dAppAddress) shouldBe dAppInitBalance
    miner.wavesBalance(callerAddress) shouldBe callerInitBalance - fee
    miner.wavesBalance(receiverAddress) shouldBe receiverInitBalance

    miner.assetBalance(dAppAddress, smartAssetId).balance shouldBe paymentAmount - 1
    miner.assetBalance(callerAddress, smartAssetId).balance shouldBe assetQuantity - paymentAmount
    miner.assetBalance(receiverAddress, smartAssetId).balance shouldBe 1
  }

  test("dApp can't transfer payed smart asset if it rejects transfers and its own balance is 0") {
    dAppInitBalance = miner.wavesBalance(dAppAddress)
    callerInitBalance = miner.wavesBalance(callerAddress)
    receiverInitBalance = miner.wavesBalance(receiverAddress)
    val paymentAmount = 10
    val fee           = smartMinFee + smartFee * 2

    assertApiError(
      invoke("resendPayment", paymentAmount, issued(rejAssetId), fee),
      AssertiveApiError(TransactionNotAllowedByAssetScript.Id, "Transaction is not allowed by token-script")
    )
  }

  test("dApp can transfer payed Waves if its own balance is 0") {
    dAppInitBalance = miner.wavesBalance(dAppAddress)
    callerInitBalance = miner.wavesBalance(callerAddress)
    receiverInitBalance = miner.wavesBalance(receiverAddress)
    dAppInitBalance shouldBe 0

    val paymentAmount = 10
    invoke("resendPayment", paymentAmount)

    miner.wavesBalance(dAppAddress) shouldBe dAppInitBalance + paymentAmount - 1
    miner.wavesBalance(callerAddress) shouldBe callerInitBalance - paymentAmount - smartMinFee
    miner.wavesBalance(receiverAddress) shouldBe receiverInitBalance + 1
  }

  def issued(assetId: String): Asset = IssuedAsset(ByteStr.decodeBase58(assetId).get)

  def invoke(func: String, amount: Long, asset: Asset = Waves, fee: Long = 500000): String = {
    miner
      .invokeScript(
        caller,
        dAppAddress,
        Some(func),
        payment = Seq(Payment(amount, asset)),
        fee = fee,
        waitForTx = true
      )
      ._1
      .id
  }

}

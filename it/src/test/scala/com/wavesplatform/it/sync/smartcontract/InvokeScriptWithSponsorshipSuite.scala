package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.minFee
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class InvokeScriptWithSponsorshipSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val dApp   = pkByAddress(firstAddress)
  private val caller = pkByAddress(secondAddress)

  val quantity: Long          = 10000
  var dAppAsset: String       = ""
  var callerAsset: String     = ""
  var dAppInitBalance: Long   = 0
  var callerInitBalance: Long = 0

  test("_send waves to dApp and caller accounts") {
    val dAppTransferId   = sender.transfer(sender.address, dApp.address, 5.waves, minFee).id
    val callerTransferId = sender.transfer(sender.address, caller.address, 5.waves, minFee).id

    nodes.waitForHeightAriseAndTxPresent(callerTransferId)
    nodes.waitForTransaction(dAppTransferId)
  }

  test("_issue and transfer assets") {
    dAppAsset = sender.issue(dApp.address, "dApp", "d", quantity, 0, reissuable = true).id
    callerAsset = sender.issue(caller.address, "caller", "c", quantity, 0, reissuable = true).id

    nodes.waitForHeightAriseAndTxPresent(callerAsset)
    sender.waitForTransaction(dAppAsset)

    val dAppToCallerId  = sender.transfer(dApp.address, caller.address, quantity / 2, minFee, Some(dAppAsset)).id
    val callerToSmartId = sender.transfer(caller.address, dApp.address, quantity / 2, minFee, Some(callerAsset)).id

    nodes.waitForHeightAriseAndTxPresent(callerToSmartId)
    sender.waitForTransaction(dAppToCallerId)
  }

  test("_enable sponsorship") {
    val dAppSponsor   = sender.sponsorAsset(dApp.address, dAppAsset, 1).id
    val callerSponsor = sender.sponsorAsset(caller.address, callerAsset, 1).id

    nodes.waitForHeightAriseAndTxPresent(callerSponsor)
    sender.waitForTransaction(dAppSponsor)
  }

  test("_set scripts to dApp and caller account") {
    val dAppScript        = ScriptCompiler.compile(s"""
          |{-# STDLIB_VERSION 3 #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |let dAppAsset = base58'$dAppAsset'
          |let callerAsset = base58'$callerAsset'
          |
          |@Callable(i)
          |func payCallerGetDAppAsset() = {
          |  if (isDefined(i.payment) && extract(i.payment).assetId == callerAsset) then
          |    TransferSet([
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset)
          |    ])
          |  else throw("need payment in callerAsset " + toBase58String(callerAsset))
          |}
        """.stripMargin).explicitGet()._1
    val dAppSetScriptTxId = sender.setScript(dApp.address, Some(dAppScript.bytes().base64)).id

    val callerScript        = ScriptCompiler.compile(s"""
          |{-# STDLIB_VERSION 3 #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |let callerAsset = base58'$callerAsset'
          |
          |@Verifier(tx)
          |func verify() = {
          |  match (tx) {
          |    case tx:InvokeScriptTransaction =>
          |      isDefined(tx.payment) && extract(tx.payment).amount > 0
          |    case _ => false
          |  }
          |}
        """.stripMargin).explicitGet()._1
    val callerSetScriptTxId = sender.setScript(caller.address, Some(callerScript.bytes().base64)).id

    nodes.waitForHeightAriseAndTxPresent(callerSetScriptTxId)
    sender.waitForTransaction(dAppSetScriptTxId)

    val dAppScriptInfo = sender.addressScriptInfo(dApp.address)
    dAppScriptInfo.script.isEmpty shouldBe false
    dAppScriptInfo.scriptText.isEmpty shouldBe false
    dAppScriptInfo.script.get.startsWith("base64:") shouldBe true
    val smartCallerScriptInfo = sender.addressScriptInfo(caller.address)
    smartCallerScriptInfo.script.isEmpty shouldBe false
    smartCallerScriptInfo.scriptText.isEmpty shouldBe false
    smartCallerScriptInfo.script.get.startsWith("base64:") shouldBe true
  }

  test("required fee is 9 callerAsset tokens ((0.005 + 0.004 fee) / 0.001 sponsored fee)") {
    val paymentAmount = 10
    val feeAmount     = 9
    assertBadRequestAndMessage(
      sender.invokeScript(
        caller.address,
        dApp.address,
        "payCallerGetDAppAsset",
        payment = Seq(Payment(paymentAmount, IssuedAsset(ByteStr.decodeBase58(callerAsset).get))),
        fee = feeAmount - 1,
        feeAssetId = Some(callerAsset)
      ),
      s"does not exceed minimal value of 900000 WAVES: ${feeAmount - 1}"
    )

    val invokeScriptTxId = sender
      .invokeScript(
        caller.address,
        dApp.address,
        "payCallerGetDAppAsset",
        payment = Seq(Payment(paymentAmount, IssuedAsset(ByteStr.decodeBase58(callerAsset).get))),
        fee = feeAmount,
        feeAssetId = Some(callerAsset)
      )
      .id

    nodes.waitForHeightAriseAndTxPresent(invokeScriptTxId)
  }

}

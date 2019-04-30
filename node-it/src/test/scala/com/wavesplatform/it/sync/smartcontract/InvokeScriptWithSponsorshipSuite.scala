package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{minFee, smartMinFee}
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
  val halfQuantity: Long      = quantity / 2
  var dAppAsset: String       = ""
  var callerAsset: String     = ""
  var smartAsset: String      = ""
  var dAppInitBalance: Long   = 0
  var callerInitBalance: Long = 0

  test("_send waves to dApp and caller accounts") {
    val dAppTransferId   = sender.transfer(sender.address, dApp.address, 5.waves, minFee).id
    val callerTransferId = sender.transfer(sender.address, caller.address, 5.waves, minFee).id

    nodes.waitForHeightAriseAndTxPresent(callerTransferId)
    nodes.waitForTransaction(dAppTransferId)
  }

  test("_issue and transfer assets") {
    dAppAsset = sender.issue(dApp.address, "dApp", "d", quantity, 0).id
    callerAsset = sender.issue(caller.address, "caller", "c", quantity, 0).id
    val script = Some(ScriptCompiler.compile("true").explicitGet()._1.bytes.value.base64)
    smartAsset = sender.issue(dApp.address, "Smart", "s", quantity, 0, script = script).id

    nodes.waitForHeightAriseAndTxPresent(callerAsset)
    sender.waitForTransaction(dAppAsset)

    val dAppToCallerId  = sender.transfer(dApp.address, caller.address, halfQuantity, minFee, Some(dAppAsset)).id
    val callerToDAppId  = sender.transfer(caller.address, dApp.address, halfQuantity, minFee, Some(callerAsset)).id
    val smartToCallerId = sender.transfer(dApp.address, caller.address, halfQuantity, smartMinFee, Some(smartAsset)).id

    nodes.waitForHeightAriseAndTxPresent(smartToCallerId)
    sender.waitForTransaction(callerToDAppId)
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
          |let smartAsset = base58'$smartAsset'
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
          |
          |@Callable(i)
          |func spendMaxFee() = {
          |  if (isDefined(i.payment) && extract(i.payment).assetId == smartAsset) then
          |    TransferSet([
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset)
          |    ])
          |  else throw("need payment in smartAsset " + toBase58String(smartAsset))
          |}
        """.stripMargin).explicitGet()._1
    val dAppSetScriptTxId = sender.setScript(dApp.address, Some(dAppScript.bytes().base64)).id

    val callerScript        = ScriptCompiler.compile(s"""
          |{-# STDLIB_VERSION 3 #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |@Verifier(tx)
          |func verify() = {
          |  let callerAsset = base58'$callerAsset'
          |  let smartAsset = base58'$smartAsset'
          |  match (tx) {
          |    case tx:InvokeScriptTransaction =>
          |      let pay = extract(tx.payment)
          |      pay.assetId == callerAsset || pay.assetId == smartAsset
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

  test("required fee in sponsored assets considers scripts count") {
    dAppInitBalance = sender.accountBalances(dApp.address)._1
    callerInitBalance = sender.accountBalances(caller.address)._1

    val paymentAmount  = 1
    val feeAmount      = 9
    val smartFeeAmount = 53

    assertBadRequestAndMessage(
      sender.invokeScript(
        caller.address,
        dApp.address,
        Some("payCallerGetDAppAsset"),
        payment = Seq(Payment(paymentAmount, IssuedAsset(ByteStr.decodeBase58(callerAsset).get))),
        fee = feeAmount - 1,
        feeAssetId = Some(dAppAsset)
      ),
      s"does not exceed minimal value of 900000 WAVES: ${feeAmount - 1}"
    )
    assertBadRequestAndMessage(
      sender.invokeScript(
        caller.address,
        dApp.address,
        Some("spendMaxFee"),
        payment = Seq(Payment(paymentAmount, IssuedAsset(ByteStr.decodeBase58(smartAsset).get))),
        fee = smartFeeAmount - 1,
        feeAssetId = Some(dAppAsset)
      ),
      s"does not exceed minimal value of 5300000 WAVES: ${smartFeeAmount - 1}"
    )

    val invokeScript1TxId = sender
      .invokeScript(
        caller.address,
        dApp.address,
        Some("payCallerGetDAppAsset"),
        payment = Seq(Payment(paymentAmount, IssuedAsset(ByteStr.decodeBase58(callerAsset).get))),
        fee = feeAmount,
        feeAssetId = Some(dAppAsset)
      )
      .id
    val invokeScript2TxId = sender
      .invokeScript(
        caller.address,
        dApp.address,
        Some("spendMaxFee"),
        payment = Seq(Payment(paymentAmount, IssuedAsset(ByteStr.decodeBase58(smartAsset).get))),
        fee = smartFeeAmount,
        feeAssetId = Some(dAppAsset)
      )
      .id

    nodes.waitForHeightAriseAndTxPresent(invokeScript2TxId)
    sender.waitForTransaction(invokeScript1TxId)

    sender.assetBalance(dApp.address, dAppAsset).balance shouldBe halfQuantity + (feeAmount - 10) + smartFeeAmount
    sender.assetBalance(dApp.address, callerAsset).balance shouldBe halfQuantity + paymentAmount
    sender.accountBalances(dApp.address)._1 shouldBe dAppInitBalance - 0.009.waves - 0.053.waves

    sender.assetBalance(caller.address, dAppAsset).balance shouldBe halfQuantity + (-feeAmount + 10) - smartFeeAmount
    sender.assetBalance(caller.address, callerAsset).balance shouldBe halfQuantity - paymentAmount
    sender.accountBalances(caller.address)._1 shouldBe callerInitBalance
  }

}

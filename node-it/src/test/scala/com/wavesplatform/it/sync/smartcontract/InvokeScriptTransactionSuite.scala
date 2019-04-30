package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{minFee, setScriptFee}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, FUNCTION_CALL}
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{DataTransaction, Proofs}
import org.scalatest.CancelAfterFailure
import play.api.libs.json.{JsNumber, Json}

class InvokeScriptTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val contract = pkByAddress(firstAddress)
  private val caller   = pkByAddress(secondAddress)

  test("setup contract account with waves") {
    sender
      .transfer(
        sender.address,
        recipient = contract.address,
        assetId = None,
        amount = 5.waves,
        fee = minFee,
        waitForTx = true
      )
      .id
  }

  test("setup caller account with waves") {
    sender
      .transfer(
        sender.address,
        recipient = contract.address,
        assetId = None,
        amount = 5.waves,
        fee = minFee,
        waitForTx = true
      )
      .id
  }

  test("set contract to contract account") {
    val scriptText =
      """
        |{-# STDLIB_VERSION 3 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |
        | @Callable(inv)
        | func foo(a:ByteVector) = {
        |  WriteSet([DataEntry("a", a), DataEntry("sender", inv.caller.bytes)])
        | }
        |
        | @Default(inv)
        | func default() = {
        |  WriteSet([DataEntry("a", "b"), DataEntry("sender", "senderId")])
        | }
        | 
        | @Verifier(t)
        | func verify() = {
        |  true
        | }
        |
        |
        """.stripMargin
    val script = ScriptCompiler.compile(scriptText).explicitGet()._1.bytes().base64
    val setScriptId = sender.setScript(contract.address, Some(script), setScriptFee, waitForTx = true).id

    val acc0ScriptInfo = sender.addressScriptInfo(contract.address)

    acc0ScriptInfo.script.isEmpty shouldBe false
    acc0ScriptInfo.scriptText.isEmpty shouldBe false
    acc0ScriptInfo.script.get.startsWith("base64:") shouldBe true

    sender.transactionInfo(setScriptId).script.get.startsWith("base64:") shouldBe true
  }

  test("contract caller invokes a function on a contract") {
    val arg               = ByteStr(Array(42: Byte))

    val invokeScriptId = sender.invokeScript(
      caller.address,
      contract.address,
      func = Some("foo"),
      args = List(CONST_BYTESTR(arg)),
      payment = Seq(),
      fee = 1.waves,
      waitForTx = true
    )

    sender.getData(contract.address, "a") shouldBe BinaryDataEntry("a", arg)
    sender.getData(contract.address, "sender") shouldBe BinaryDataEntry("sender", caller.toAddress.bytes)
  }

  test("contract caller invokes a default function on a contract") {


    val invokeScriptId = sender.invokeScript(
      caller.address,
      contract.address,
      func = None,
      payment = Seq(),
      fee = 1.waves,
      waitForTx = true
    )
    sender.getData(contract.address, "a") shouldBe StringDataEntry("a", "b")
    sender.getData(contract.address, "sender") shouldBe StringDataEntry("sender", "senderId")
  }

  test("verifier works") {

    val tx =
      DataTransaction
        .create(
          sender = contract,
          data = List(StringDataEntry("a", "OOO")),
          feeAmount = 1.waves,
          timestamp = System.currentTimeMillis(),
          proofs = Proofs.empty
        )
        .explicitGet()

    val dataTxId = sender
      .signedBroadcast(tx.json() + ("type" -> JsNumber(DataTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(dataTxId)

    sender.getData(contract.address, "a") shouldBe StringDataEntry("a", "OOO")
  }
}

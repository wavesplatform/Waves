package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{minFee, setScriptFee}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, FUNCTION_CALL}
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer._
import org.scalatest.CancelAfterFailure
import play.api.libs.json.{JsNumber, Json}

class HodlContractTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val contract = pkByAddress(firstAddress)
  private val caller   = pkByAddress(secondAddress)

  test("setup contract account with waves") {
    val tx =
      TransferTransactionV2
        .selfSigned(
          assetId = Waves,
          sender = sender.privateKey,
          recipient = contract,
          amount = 5.waves,
          timestamp = System.currentTimeMillis(),
          feeAssetId = Waves,
          feeAmount = minFee,
          attachment = Array.emptyByteArray
        )
        .explicitGet()

    val transferId = sender
      .signedBroadcast(tx.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt)))
      .id
    nodes.waitForHeightAriseAndTxPresent(transferId)
  }

  test("setup caller account with waves") {
    val tx =
      TransferTransactionV2
        .selfSigned(
          assetId = Waves,
          sender = sender.privateKey,
          recipient = caller,
          amount = 10.waves,
          timestamp = System.currentTimeMillis(),
          feeAssetId = Waves,
          feeAmount = minFee,
          attachment = Array.emptyByteArray
        )
        .explicitGet()

    val transferId = sender
      .signedBroadcast(tx.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt)))
      .id
    nodes.waitForHeightAriseAndTxPresent(transferId)
  }

  test("set contract to contract account") {
    val scriptText =
      """
        |{-# STDLIB_VERSION 3 #-}
        |{-# CONTENT_TYPE CONTRACT #-}
        |
        |	@Callable(i)
        |	func deposit() = {
        |   let pmt = extract(i.payment)
        |   if (isDefined(pmt.asset)) then throw("can hodl waves only at the moment")
        |   else {
        |	  	let currentKey = toBase58String(i.caller.bytes)
        |	  	let currentAmount = match getInteger(this, currentKey) {
        |	  		case a:Int => a
        |	  		case _ => 0
        |	  	}
        |	  	let newAmount = currentAmount + pmt.amount
        |	  	WriteSet([DataEntry(currentKey, newAmount)])
        |
        |   }
        |	}
        |
        | @Callable(i)
        | func withdraw(amount: Int) = {
        |	  	let currentKey = toBase58String(i.caller.bytes)
        |	  	let currentAmount = match getInteger(this, currentKey) {
        |	  		case a:Int => a
        |	  		case _ => 0
        |	  	}
        |		let newAmount = currentAmount - amount
        |	 if (amount < 0)
        |			then throw("Can't withdraw negative amount")
        |  else if (newAmount < 0)
        |			then throw("Not enough balance")
        |			else ContractResult(
        |					WriteSet([DataEntry(currentKey, newAmount)]),
        |					TransferSet([ContractTransfer(i.caller, amount, unit)])
        |				)
        |	}
        |
        |
        |
        """.stripMargin

    val script = ScriptCompiler.compile(scriptText).explicitGet()._1
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(contract, Some(script), setScriptFee, System.currentTimeMillis())
      .explicitGet()

    val setScriptId = sender
      .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val acc0ScriptInfo = sender.addressScriptInfo(contract.address)

    acc0ScriptInfo.script.isEmpty shouldBe false
    acc0ScriptInfo.scriptText.isEmpty shouldBe false
    acc0ScriptInfo.script.get.startsWith("base64:") shouldBe true

    val json = Json.parse(sender.get(s"/transactions/info/$setScriptId").getResponseBody)
    (json \ "script").as[String].startsWith("base64:") shouldBe true
  }

  test("caller deposits waves") {
    val balanceBefore = sender.accountBalances(contract.address)._1
    val tx =
      InvokeScriptTransaction
        .selfSigned(
          sender = caller,
          contractAddress = contract,
          fc = FUNCTION_CALL(FunctionHeader.User("deposit"), List.empty),
          p = Seq(InvokeScriptTransaction.Payment(1.5.waves, Waves)),
          timestamp = System.currentTimeMillis(),
          fee = 1.waves,
          feeAssetId = Waves
        )
        .explicitGet()

    val contractInvocationId = sender
      .signedBroadcast(tx.json() + ("type" -> JsNumber(InvokeScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(contractInvocationId)

    sender.getData(contract.address, caller.address) shouldBe IntegerDataEntry(caller.address, 1.5.waves)
    val balanceAfter = sender.accountBalances(contract.address)._1

    (balanceAfter - balanceBefore) shouldBe 1.5.waves
  }

  test("caller can't withdraw more than owns") {
    val tx =
      InvokeScriptTransaction
        .selfSigned(
          sender = caller,
          contractAddress = contract,
          fc = FUNCTION_CALL(FunctionHeader.User("withdraw"), List(CONST_LONG(1.51.waves))),
          p = Seq(),
          timestamp = System.currentTimeMillis(),
          fee = 1.waves,
          feeAssetId = Waves
        )
        .explicitGet()

    assertBadRequestAndMessage(sender
                                 .signedBroadcast(tx.json() + ("type" -> JsNumber(InvokeScriptTransaction.typeId.toInt))),
                               "Not enough balance")
  }

  test("caller can withdraw less than he owns") {
    val balanceBefore = sender.accountBalances(contract.address)._1
    val tx =
      InvokeScriptTransaction
        .selfSigned(
          sender = caller,
          contractAddress = contract,
          fc = FUNCTION_CALL(FunctionHeader.User("withdraw"), List(CONST_LONG(1.49.waves))),
          p = Seq(),
          timestamp = System.currentTimeMillis(),
          fee = 1.waves,
          feeAssetId = Waves
        )
        .explicitGet()

    val contractInvocationId = sender
      .signedBroadcast(tx.json() + ("type" -> JsNumber(InvokeScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(contractInvocationId)

    val balanceAfter = sender.accountBalances(contract.address)._1

    sender.getData(contract.address, caller.address) shouldBe IntegerDataEntry(caller.address, 0.01.waves)
    (balanceAfter - balanceBefore) shouldBe -1.49.waves
  }
}

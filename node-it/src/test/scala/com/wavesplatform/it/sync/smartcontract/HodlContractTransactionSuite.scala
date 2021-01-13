package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{PutDataResponse, TransactionInfo}
import com.wavesplatform.it.sync.{minFee, setScriptFee}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms.CONST_LONG
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class HodlContractTransactionSuite extends BaseTransactionSuite {

  private def contract = firstKeyPair

  private def caller = secondKeyPair

  private lazy val contractAddress: String = contract.toAddress.toString
  private lazy val callerAddress: String = caller.toAddress.toString

  test("setup contract account with waves") {
    miner
      .transfer(
        miner.keyPair,
        recipient = contractAddress,
        assetId = None,
        amount = 5.waves,
        fee = minFee,
        waitForTx = true
      )
      .id
  }

  test("setup caller account with waves") {
    miner
      .transfer(
        miner.keyPair,
        recipient = callerAddress,
        assetId = None,
        amount = 10.waves,
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
        |	@Callable(i)
        |	func deposit() = {
        |   let pmt = extract(i.payment)
        |   if (isDefined(pmt.assetId)) then throw("can hodl waves only at the moment")
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
        |			else  ScriptResult(
        |					WriteSet([DataEntry(currentKey, newAmount)]),
        |					TransferSet([ScriptTransfer(i.caller, amount, unit)])
        |				)
        |	}
        """.stripMargin

    val script      = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    val setScriptId = miner.setScript(contract, Some(script), setScriptFee, waitForTx = true).id

    val acc0ScriptInfo = miner.addressScriptInfo(contractAddress)

    acc0ScriptInfo.script.isEmpty shouldBe false
    acc0ScriptInfo.scriptText.isEmpty shouldBe false
    acc0ScriptInfo.script.get.startsWith("base64:") shouldBe true

    miner.transactionInfo[TransactionInfo](setScriptId).script.get.startsWith("base64:") shouldBe true
  }

  test("caller deposits waves") {
    val balanceBefore = miner.wavesBalance(contractAddress)
    val invokeScriptId = miner
      .invokeScript(
        caller,
        dappAddress = contractAddress,
        func = Some("deposit"),
        args = List.empty,
        payment = Seq(InvokeScriptTransaction.Payment(1.5.waves, Waves)),
        fee = 1.waves,
        waitForTx = true
      )
      ._1
      .id

    miner.waitForTransaction(invokeScriptId)

    miner.getDataByKey(contractAddress, callerAddress) shouldBe IntegerDataEntry(callerAddress, 1.5.waves)
    val balanceAfter = miner.wavesBalance(contractAddress)
    (balanceAfter - balanceBefore) shouldBe 1.5.waves
  }

  test("caller can't withdraw more than owns") {
    assertBadRequestAndMessage(
      miner
        .invokeScript(
          caller,
          contractAddress,
          func = Some("withdraw"),
          args = List(CONST_LONG(1.51.waves)),
          payment = Seq(),
          fee = 1.waves
        ),
      "Not enough balance"
    )
  }

  test("caller can withdraw less than he owns") {
    val balanceBefore = miner.wavesBalance(contractAddress)
    val invokeScriptId = miner
      .invokeScript(
        caller,
        dappAddress = contractAddress,
        func = Some("withdraw"),
        args = List(CONST_LONG(1.49.waves)),
        payment = Seq(),
        fee = 1.waves,
        waitForTx = true
      )
      ._1
      .id

    val balanceAfter = miner.wavesBalance(contractAddress)
    miner.getDataByKey(contractAddress, callerAddress) shouldBe IntegerDataEntry(callerAddress, 0.01.waves)
    (balanceAfter - balanceBefore) shouldBe -1.49.waves

    val stateChangesInfo = miner.debugStateChanges(invokeScriptId).stateChanges

    val stateChangesData = stateChangesInfo.get.data.head.asInstanceOf[PutDataResponse]
    stateChangesInfo.get.data.length shouldBe 1
    stateChangesData.`type` shouldBe "integer"
    stateChangesData.value.asInstanceOf[Long] shouldBe 0.01.waves

    val stateChangesTransfers = stateChangesInfo.get.transfers.head
    stateChangesInfo.get.transfers.length shouldBe 1
    stateChangesTransfers.address shouldBe callerAddress
    stateChangesTransfers.amount shouldBe 1.49.waves
    stateChangesTransfers.asset shouldBe None
  }

}

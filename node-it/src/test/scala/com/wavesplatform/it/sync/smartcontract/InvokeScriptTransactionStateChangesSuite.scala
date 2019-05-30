package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api
import com.wavesplatform.it.api.{DataResponse, DebugStateChanges, StateChangesDetails, TransactionInfo}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.setScriptFee
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms.CONST_LONG
import com.wavesplatform.state._
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class InvokeScriptTransactionStateChangesSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val contract = pkByAddress(firstAddress)
  private val caller = pkByAddress(secondAddress)
  private val recipient = pkByAddress(thirdAddress)

  test("write") {
    val simpleAsset = sender.issue(contract.address, "simple", "", 1000, 0)

    val script = ScriptCompiler.compile(
      """
        |{-# STDLIB_VERSION 3 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |
        |@Callable(i)
        |func write(text: String) = {
        |    WriteSet([DataEntry("result", text)])
        |}
        |
        |@Callable(i)
        |func sendWaves(recipient: String, amount: Int) = {
        |    TransferSet([ScriptTransfer(Address(recipient.fromBase58String()), amount, unit)])
        |}
        |
        |@Callable(i)
        |func sendAsset(recipient: String, amount: Int, assetId: String) = {
        |    TransferSet([ScriptTransfer(Address(recipient.fromBase58String()), amount, assetId.fromBase58String())])
        |}
        |
        |@Callable(i)
        |func writeAndSendWaves(text: String, recipient: String, amount: Int) = {
        |    ScriptResult(
        |        WriteSet([DataEntry("result", text)]),
        |        TransferSet([ScriptTransfer(Address(recipient.fromBase58String()), amount, unit)])
        |    )
        |}
        |
        |@Callable(i)
        |func writeAndSendAsset(text: String, recipient: String, amount: Int, assetId: String) = {
        |    ScriptResult(
        |        WriteSet([DataEntry("result", text)]),
        |        TransferSet([ScriptTransfer(Address(recipient.fromBase58String()), amount, assetId.fromBase58String())])
        |    )
        |}
      """.stripMargin).explicitGet()._1.bytes().base64
    sender.setScript(contract.address, Some(script), setScriptFee, waitForTx = true).id

    val initCallerTxs = sender.transactionsByAddress(caller.address, 100).length
    val initDAppTxs = sender.transactionsByAddress(contract.address, 100).length
    val initCallerStateChanges = sender.debugStateChangesByAddress(caller.address, 100).length
    val initDAppStateChanges = sender.debugStateChangesByAddress(contract.address, 100).length
    initCallerTxs shouldBe initCallerStateChanges
    initDAppTxs shouldBe initDAppStateChanges



    val writeTx = sender.invokeScript(
      caller.address,
      contract.address,
      func = Some("write"),
      args = List(CONST_LONG(10)),
      fee = 0.005.waves,
      waitForTx = true
    )

    val txInfo = sender.transactionInfo(writeTx.id)
    val callerTxs = sender.transactionsByAddress(caller.address, 100)
    val dAppTxs = sender.transactionsByAddress(contract.address, 100)
    val txStateChanges = sender.debugStateChanges(writeTx.id)
    val callerStateChanges = sender.debugStateChangesByAddress(caller.address, 100)
    val dAppStateChanges = sender.debugStateChangesByAddress(contract.address, 100)

    callerTxs.length shouldBe initCallerTxs + 1
    callerTxs.length shouldBe callerStateChanges.length
    dAppTxs.length shouldBe initDAppTxs + 1
    dAppTxs.length shouldBe dAppStateChanges.length

    txInfoShouldBeEqual(txInfo, txStateChanges)

    val expected = StateChangesDetails(Seq(DataResponse("string", 10, "result")), Seq())
    txStateChanges.stateChanges.get.data shouldBe expected
    callerStateChanges.head.stateChanges.get shouldBe expected
    dAppStateChanges.head.stateChanges.get shouldBe expected
  }

  /*test("contract caller invokes a default function on a contract") {


    val _ = sender.invokeScript(
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
  }*/

  def txInfoShouldBeEqual(info: TransactionInfo, stateChanges: DebugStateChanges) {
    info.`type` shouldBe stateChanges.`type`
    info.id shouldBe stateChanges.id
    info.fee shouldBe stateChanges.fee
    info.timestamp shouldBe stateChanges.timestamp
    info.sender shouldBe stateChanges.sender
    info.height shouldBe stateChanges.height
    info.minSponsoredAssetFee shouldBe stateChanges.minSponsoredAssetFee
    info.script shouldBe stateChanges.script
  }
}

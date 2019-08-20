package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.{DataResponse, DebugStateChanges, StateChangesDetails, TransactionInfo, TransfersInfoResponse}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.setScriptFee
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, CONST_STRING}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import org.scalatest.CancelAfterFailure

class InvokeScriptTransactionStateChangesSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val contract  = pkByAddress(firstAddress)
  private val caller    = pkByAddress(secondAddress)
  private val recipient = pkByAddress(thirdAddress)

  var simpleAsset: String               = ""
  var assetSponsoredByDApp: String      = ""
  var assetSponsoredByRecipient: String = ""
  var initCallerTxs: Long               = 0
  var initDAppTxs: Long                 = 0
  var initRecipientTxs: Long            = 0
  var initCallerStateChanges: Long      = 0
  var initDAppStateChanges: Long        = 0
  var initRecipientStateChanges: Long   = 0

  test("prepare") {
    simpleAsset = sender.issue(contract.address, "simple", "", 9000, 0).id
    assetSponsoredByDApp = sender.issue(contract.address, "DApp asset", "", 9000, 0).id
    assetSponsoredByRecipient = sender.issue(recipient.address, "Recipient asset", "", 9000, 0, waitForTx = true).id
    sender.massTransfer(contract.address, List(Transfer(caller.address, 3000), Transfer(recipient.address, 3000)), 0.01.waves, Some(simpleAsset))
    sender.massTransfer(contract.address,
                        List(Transfer(caller.address, 3000), Transfer(recipient.address, 3000)),
                        0.01.waves,
                        Some(assetSponsoredByDApp))
    sender.massTransfer(recipient.address,
                        List(Transfer(caller.address, 3000), Transfer(contract.address, 3000)),
                        0.01.waves,
                        Some(assetSponsoredByRecipient))
    sender.sponsorAsset(contract.address, assetSponsoredByDApp, 1)
    sender.sponsorAsset(recipient.address, assetSponsoredByRecipient, 5)

    val script = ScriptCompiler.compile("""
        |{-# STDLIB_VERSION 3 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |
        |@Callable(i)
        |func write(value: Int) = {
        |    WriteSet([DataEntry("result", value)])
        |}
        |
        |@Callable(i)
        |func sendAsset(recipient: String, amount: Int, assetId: String) = {
        |    TransferSet([ScriptTransfer(Address(recipient.fromBase58String()), amount, assetId.fromBase58String())])
        |}
        |
        |@Callable(i)
        |func writeAndSendWaves(value: Int, recipient: String, amount: Int) = {
        |    ScriptResult(
        |        WriteSet([DataEntry("result", value)]),
        |        TransferSet([ScriptTransfer(Address(recipient.fromBase58String()), amount, unit)])
        |    )
        |}
      """.stripMargin).explicitGet()._1.bytes().base64
    sender.setScript(contract.address, Some(script), setScriptFee, waitForTx = true)

    initCallerTxs = sender.transactionsByAddress(caller.address, 100).length
    initDAppTxs = sender.transactionsByAddress(contract.address, 100).length
    initRecipientTxs = sender.transactionsByAddress(recipient.address, 100).length
    initCallerStateChanges = sender.debugStateChangesByAddress(caller.address, 100).length
    initDAppStateChanges = sender.debugStateChangesByAddress(contract.address, 100).length
    initRecipientStateChanges = sender.debugStateChangesByAddress(recipient.address, 100).length
    initCallerTxs shouldBe initCallerStateChanges
    initDAppTxs shouldBe initDAppStateChanges
    initRecipientTxs shouldBe initRecipientStateChanges
  }

  test("write") {
    val invokeTx = sender.invokeScript(
      caller.address,
      contract.address,
      func = Some("write"),
      args = List(CONST_LONG(10)),
      fee = 0.005.waves,
      waitForTx = true
    )

    val txInfo             = sender.transactionInfo(invokeTx.id)
    val callerTxs          = sender.transactionsByAddress(caller.address, 100)
    val dAppTxs            = sender.transactionsByAddress(contract.address, 100)
    val txStateChanges     = sender.debugStateChanges(invokeTx.id)
    val callerStateChanges = sender.debugStateChangesByAddress(caller.address, 100)
    val dAppStateChanges   = sender.debugStateChangesByAddress(contract.address, 100)

    callerTxs.length shouldBe initCallerTxs + 1
    callerTxs.length shouldBe callerStateChanges.length
    dAppTxs.length shouldBe initDAppTxs + 1
    dAppTxs.length shouldBe dAppStateChanges.length

    txInfoShouldBeEqual(txInfo, txStateChanges)

    val expected = StateChangesDetails(Seq(DataResponse("integer", 10, "result")), Seq())
    txStateChanges.stateChanges.get shouldBe expected
    callerStateChanges.head.stateChanges.get shouldBe expected
    dAppStateChanges.head.stateChanges.get shouldBe expected
  }

  test("sponsored by dApp") {
    val invokeTx = sender.invokeScript(
      caller.address,
      contract.address,
      func = Some("sendAsset"),
      args = List(
        CONST_STRING(recipient.address).explicitGet(),
        CONST_LONG(10),
        CONST_STRING(simpleAsset).explicitGet()
      ),
      fee = 5,
      feeAssetId = Some(assetSponsoredByDApp),
      waitForTx = true
    )

    val txInfo                = sender.transactionInfo(invokeTx.id)
    val callerTxs             = sender.transactionsByAddress(caller.address, 100)
    val dAppTxs               = sender.transactionsByAddress(contract.address, 100)
    val recipientTxs          = sender.transactionsByAddress(recipient.address, 100)
    val txStateChanges        = sender.debugStateChanges(invokeTx.id)
    val callerStateChanges    = sender.debugStateChangesByAddress(caller.address, 100)
    val dAppStateChanges      = sender.debugStateChangesByAddress(contract.address, 100)
    val recipientStateChanges = sender.debugStateChangesByAddress(recipient.address, 100)

    callerTxs.length shouldBe initCallerTxs + 2
    callerTxs.length shouldBe callerStateChanges.length
    dAppTxs.length shouldBe initDAppTxs + 2
    dAppTxs.length shouldBe dAppStateChanges.length
    recipientTxs.length shouldBe initRecipientTxs + 1
    recipientTxs.length shouldBe recipientStateChanges.length

    txInfoShouldBeEqual(txInfo, txStateChanges)
    txInfoShouldBeEqual(callerTxs.head, txStateChanges)
    txInfoShouldBeEqual(dAppTxs.head, txStateChanges)
    txInfoShouldBeEqual(recipientTxs.head, txStateChanges)
    txInfoShouldBeEqual(txInfo, callerStateChanges.head)
    txInfoShouldBeEqual(txInfo, dAppStateChanges.head)
    txInfoShouldBeEqual(txInfo, recipientStateChanges.head)

    val expected = StateChangesDetails(Seq(), Seq(TransfersInfoResponse(recipient.address, Some(simpleAsset), 10)))
    txStateChanges.stateChanges.get shouldBe expected
    callerStateChanges.head.stateChanges.get shouldBe expected
    dAppStateChanges.head.stateChanges.get shouldBe expected
    recipientStateChanges.head.stateChanges.get shouldBe expected
  }

  test("sponsored by recipient") {
    val invokeTx = sender.invokeScript(
      caller.address,
      contract.address,
      func = Some("writeAndSendWaves"),
      args = List(CONST_LONG(7), CONST_STRING(caller.address).explicitGet(), CONST_LONG(10)),
      fee = 25,
      feeAssetId = Some(assetSponsoredByRecipient),
      waitForTx = true
    )

    val txInfo                = sender.transactionInfo(invokeTx.id)
    val callerTxs             = sender.transactionsByAddress(caller.address, 100)
    val dAppTxs               = sender.transactionsByAddress(contract.address, 100)
    val recipientTxs          = sender.transactionsByAddress(recipient.address, 100)
    val txStateChanges        = sender.debugStateChanges(invokeTx.id)
    val callerStateChanges    = sender.debugStateChangesByAddress(caller.address, 100)
    val dAppStateChanges      = sender.debugStateChangesByAddress(contract.address, 100)
    val recipientStateChanges = sender.debugStateChangesByAddress(recipient.address, 100)

    callerTxs.length shouldBe initCallerTxs + 3
    callerTxs.length shouldBe callerStateChanges.length
    dAppTxs.length shouldBe initDAppTxs + 3
    dAppTxs.length shouldBe dAppStateChanges.length
    recipientTxs.length shouldBe initRecipientTxs + 2
    recipientTxs.length shouldBe recipientStateChanges.length

    txInfoShouldBeEqual(txInfo, txStateChanges)
    txInfoShouldBeEqual(callerTxs.head, txStateChanges)
    txInfoShouldBeEqual(dAppTxs.head, txStateChanges)
    txInfoShouldBeEqual(recipientTxs.head, txStateChanges)
    txInfoShouldBeEqual(txInfo, callerStateChanges.head)
    txInfoShouldBeEqual(txInfo, dAppStateChanges.head)
    txInfoShouldBeEqual(txInfo, recipientStateChanges.head)

    val expected = StateChangesDetails(
      Seq(DataResponse("integer", 7, "result")),
      Seq(TransfersInfoResponse(caller.address, None, 10))
    )
    txStateChanges.stateChanges.get shouldBe expected
    callerStateChanges.head.stateChanges.get shouldBe expected
    dAppStateChanges.head.stateChanges.get shouldBe expected
  }

  test("Error on wrong tx type") {
    val tx = nodes.head.transfer(
      caller.address,
      recipient.address,
      100000000,
      1000000,
      None,
      None,
      1,
      waitForTx = true
    )

    assertBadRequestAndMessage(
      nodes.head.debugStateChanges(tx.id),
      "transaction type not supported",
      expectedStatusCode = 501
    )
  }

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

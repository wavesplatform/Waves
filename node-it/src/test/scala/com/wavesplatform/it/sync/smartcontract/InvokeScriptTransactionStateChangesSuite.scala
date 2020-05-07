package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{DataResponse, DebugStateChanges, StateChangesDetails, TransactionInfo, TransfersInfoResponse}
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, CONST_STRING}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import org.scalactic.source.Position
import org.scalatest.CancelAfterFailure

class InvokeScriptTransactionStateChangesSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val contract  = firstAddress
  private val caller    = secondAddress
  private val recipient = thirdAddress

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
    simpleAsset = sender.issue(contract, "simple", "", 9000, 0).id
    assetSponsoredByDApp = sender.issue(contract, "DApp asset", "", 9000, 0).id
    assetSponsoredByRecipient = sender.issue(recipient, "Recipient asset", "", 9000, 0, waitForTx = true).id
    sender.massTransfer(contract, List(Transfer(caller, 3000), Transfer(recipient, 3000)), 0.01.waves, assetId = Some(simpleAsset))
    sender.massTransfer(contract,
                        List(Transfer(caller, 3000), Transfer(recipient, 3000)),
                        0.01.waves,
                        assetId = Some(assetSponsoredByDApp))
    sender.massTransfer(recipient,
                        List(Transfer(caller, 3000), Transfer(contract, 3000)),
                        0.01.waves,
                        assetId = Some(assetSponsoredByRecipient))
    sender.sponsorAsset(contract, assetSponsoredByDApp, 1, fee = sponsorReducedFee + smartFee)
    sender.sponsorAsset(recipient, assetSponsoredByRecipient, 5, fee = sponsorReducedFee + smartFee)

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
      """.stripMargin, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    sender.setScript(contract, Some(script), setScriptFee, waitForTx = true)

    initCallerTxs = sender.transactionsByAddress(caller, 100).length
    initDAppTxs = sender.transactionsByAddress(contract, 100).length
    initRecipientTxs = sender.transactionsByAddress(recipient, 100).length
    initCallerStateChanges = sender.debugStateChangesByAddress(caller, 100).length
    initDAppStateChanges = sender.debugStateChangesByAddress(contract, 100).length
    initRecipientStateChanges = sender.debugStateChangesByAddress(recipient, 100).length
    initCallerTxs shouldBe initCallerStateChanges
    initDAppTxs shouldBe initDAppStateChanges
    initRecipientTxs shouldBe initRecipientStateChanges
  }

  test("write") {
    val data = 10

    val invokeTx = sender.validateInvokeScript( // Since BlockV5 broadcasting InvokeTx does not return trace
      caller,
      contract,
      func = Some("write"),
      args = List(CONST_LONG(data)),
      fee = 0.005.waves,
    )

    val js = invokeTx._2

    (js \ "trace" \ 0 \ "result" \ "vars" \ 0 \ "name").as[String] shouldBe "value"
    (js \ "trace" \ 0 \ "result" \ "vars" \ 0 \ "value").as[String] shouldBe data.toString

   val id = sender.signedBroadcast(invokeTx._1, waitForTx = true).id

    nodes.waitForHeightAriseAndTxPresent(id)

    val txInfo             = sender.transactionInfo[TransactionInfo](id)

    sender.waitForHeight(txInfo.height + 1)

    val callerTxs          = sender.transactionsByAddress(caller, 100)
    val dAppTxs            = sender.transactionsByAddress(contract, 100)
    val txStateChanges     = sender.debugStateChanges(id)
    val callerStateChanges = sender.debugStateChangesByAddress(caller, 100)
    val dAppStateChanges   = sender.debugStateChangesByAddress(contract, 100)

    callerTxs.length shouldBe initCallerTxs + 1
    callerTxs.length shouldBe callerStateChanges.length
    dAppTxs.length shouldBe initDAppTxs + 1
    dAppTxs.length shouldBe dAppStateChanges.length

    txInfoShouldBeEqual(txInfo, txStateChanges)

    val expected = StateChangesDetails(Seq(DataResponse("integer", 10, "result")), Seq(), Seq(), Seq(), Seq(), None)
    txStateChanges.stateChanges.get shouldBe expected
    callerStateChanges.head.stateChanges.get shouldBe expected
    dAppStateChanges.head.stateChanges.get shouldBe expected
  }

  test("sponsored by dApp") {
    val invokeTx = sender.invokeScript(
      caller,
      contract,
      func = Some("sendAsset"),
      args = List(
        CONST_STRING(recipient).explicitGet(),
        CONST_LONG(10),
        CONST_STRING(simpleAsset).explicitGet()
      ),
      fee = 5,
      feeAssetId = Some(assetSponsoredByDApp),
      waitForTx = true
    )

    val txInfo                = sender.transactionInfo[TransactionInfo](invokeTx._1.id)
    val callerTxs             = sender.transactionsByAddress(caller, 100)
    val dAppTxs               = sender.transactionsByAddress(contract, 100)
    val recipientTxs          = sender.transactionsByAddress(recipient, 100)
    val txStateChanges        = sender.debugStateChanges(invokeTx._1.id)
    val callerStateChanges    = sender.debugStateChangesByAddress(caller, 100)
    val dAppStateChanges      = sender.debugStateChangesByAddress(contract, 100)
    val recipientStateChanges = sender.debugStateChangesByAddress(recipient, 100)

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

    val expected = StateChangesDetails(Seq(), Seq(TransfersInfoResponse(recipient, Some(simpleAsset), 10)), Seq(), Seq(), Seq(), None)
    txStateChanges.stateChanges.get shouldBe expected
    callerStateChanges.head.stateChanges.get shouldBe expected
    dAppStateChanges.head.stateChanges.get shouldBe expected
    recipientStateChanges.head.stateChanges.get shouldBe expected
  }

  test("sponsored by recipient") {
    val invokeTx = sender.invokeScript(
      caller,
      contract,
      func = Some("writeAndSendWaves"),
      args = List(CONST_LONG(7), CONST_STRING(caller).explicitGet(), CONST_LONG(10)),
      fee = 25,
      feeAssetId = Some(assetSponsoredByRecipient),
      waitForTx = true
    )

    val txInfo                = sender.transactionInfo[TransactionInfo](invokeTx._1.id)
    val callerTxs             = sender.transactionsByAddress(caller, 100)
    val dAppTxs               = sender.transactionsByAddress(contract, 100)
    val recipientTxs          = sender.transactionsByAddress(recipient, 100)
    val txStateChanges        = sender.debugStateChanges(invokeTx._1.id)
    val callerStateChanges    = sender.debugStateChangesByAddress(caller, 100)
    val dAppStateChanges      = sender.debugStateChangesByAddress(contract, 100)
    val recipientStateChanges = sender.debugStateChangesByAddress(recipient, 100)

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
      Seq(TransfersInfoResponse(caller, None, 10)),
      Seq(),
      Seq(),
      Seq(),
      None
    )
    txStateChanges.stateChanges.get shouldBe expected
    callerStateChanges.head.stateChanges.get shouldBe expected
    dAppStateChanges.head.stateChanges.get shouldBe expected
  }

  test("Error on wrong tx type") {
    val tx = nodes.head.transfer(
      caller,
      recipient,
      1.waves,
      waitForTx = true
    )

    assertBadRequestAndMessage(
      nodes.head.debugStateChanges(tx.id),
      "transaction type not supported",
      expectedStatusCode = 501
    )
  }

  def txInfoShouldBeEqual(info: TransactionInfo, stateChanges: DebugStateChanges)(implicit pos: Position) {
    info._type shouldBe stateChanges._type
    info.id shouldBe stateChanges.id
    info.fee shouldBe stateChanges.fee
    info.timestamp shouldBe stateChanges.timestamp
    info.sender shouldBe stateChanges.sender
    info.height shouldBe stateChanges.height
    info.minSponsoredAssetFee shouldBe stateChanges.minSponsoredAssetFee
    info.script shouldBe stateChanges.script
  }
}

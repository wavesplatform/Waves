package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api._
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
    sender.massTransfer(contract, List(Transfer(caller, 3000), Transfer(recipient, 3000)), 0.01.waves, assetId = Some(assetSponsoredByDApp))
    sender.massTransfer(recipient, List(Transfer(caller, 3000), Transfer(contract, 3000)), 0.01.waves, assetId = Some(assetSponsoredByRecipient))
    sender.sponsorAsset(contract, assetSponsoredByDApp, 1, fee = sponsorReducedFee + smartFee)
    sender.sponsorAsset(recipient, assetSponsoredByRecipient, 5, fee = sponsorReducedFee + smartFee)

    val script = ScriptCompiler
      .compile(
        """
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
      """.stripMargin,
        ScriptEstimatorV2
      )
      .explicitGet()
      ._1
      .bytes()
      .base64
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
      fee = 0.005.waves
    )

    val js = invokeTx._2

    (js \ "trace" \ 0 \ "result" \ "vars" \ 0 \ "name").as[String] shouldBe "value"
    (js \ "trace" \ 0 \ "result" \ "vars" \ 0 \ "value").as[String] shouldBe data.toString

    val id = sender.signedBroadcast(invokeTx._1, waitForTx = true).id

    nodes.waitForHeightAriseAndTxPresent(id)

    val txInfo = sender.transactionInfo[TransactionInfo](id)

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

    val expected = StateChangesDetails(Seq(DataResponse.put("integer", 10, "result")), Seq(), Seq(), Seq(), Seq(), Seq(), None)
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

    val expected = StateChangesDetails(Seq(), Seq(TransfersInfoResponse(recipient, Some(simpleAsset), 10)), Seq(), Seq(), Seq(), Seq(), None)
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
      Seq(DataResponse.put("integer", 7, "result")),
      Seq(TransfersInfoResponse(caller, None, 10)),
      Seq(),
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

  test("state changes order") {
    val script = ScriptCompiler
      .compile(
        s"""
      |{-# STDLIB_VERSION 4 #-}
      |{-# CONTENT_TYPE DAPP #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}

      |@Callable(i)
      |func order1() = {
      |  [
      |    ScriptTransfer(i.caller, 1, unit),
      |    IntegerEntry("a", 1),
      |    StringEntry("b", "a"),
      |    BinaryEntry("c", base58'a'),
      |    BooleanEntry("d", true),
      |    DeleteEntry("e"),
      |    ScriptTransfer(i.caller, 2, unit),
      |    BooleanEntry("d", false),
      |    DeleteEntry("e"),
      |    StringEntry("f", "a")
      |  ]
      |}
      |
      |@Callable(i)
      |func order2() = {
      |  [
      |    SponsorFee(base58'$assetSponsoredByDApp', 950),
      |    Issue("asset #1", "", 100, 8, true, unit, 0),
      |    Reissue(base58'$simpleAsset', true, 1),
      |    Burn(base58'$simpleAsset', 3),
      |    Reissue(base58'$assetSponsoredByDApp', true, 2),
      |    SponsorFee(base58'$simpleAsset', 500),
      |    Burn(base58'$assetSponsoredByDApp', 4),
      |    Issue("asset #2", "", 100, 8, true, unit, 0),
      |    SponsorFee(base58'$assetSponsoredByDApp', 1000),
      |    Reissue(base58'$simpleAsset', true, 3)
      |  ]
      |}
      """.stripMargin,
        ScriptEstimatorV2
      )
      .explicitGet()
      ._1
      .bytes()
      .base64
    sender.setScript(contract, Some(script), setScriptFee + 0.4.waves, waitForTx = true)

    val invokeTx1 = sender.invokeScript(
      caller,
      contract,
      func = Some("order1"),
      args = List.empty,
      fee = 10.waves,
      waitForTx = true
    )

    val expectedDataResponses = Seq(
      DataResponse.put("integer", 1, "a"),
      DataResponse.put("string", "a", "b"),
      DataResponse.put("binary", "base64:IQ==", "c"),
      DataResponse.put("boolean", true, "d"),
      DataResponse.delete("e"),
      DataResponse.put("boolean", false, "d"),
      DataResponse.delete("e"),
      DataResponse.put("string", "a", "f")
    )
    val expectedTransferResponses = Seq(TransfersInfoResponse(caller, None, 1), TransfersInfoResponse(caller, None, 2))

    val idStateChanges1      = sender.debugStateChanges(invokeTx1._1.id).stateChanges
    val addressStateChanges1 = sender.debugStateChangesByAddress(caller, 1).head.stateChanges

    Seq(idStateChanges1, addressStateChanges1).foreach { actualStateChanges =>
      actualStateChanges.get.data shouldBe expectedDataResponses
      actualStateChanges.get.transfers shouldBe expectedTransferResponses
    }

    val invokeTx2 = sender.invokeScript(
      caller,
      contract,
      func = Some("order2"),
      args = List.empty,
      fee = 10.waves,
      waitForTx = true
    )

    val expectedSponsorFeeResponses = Seq(
      SponsorFeeResponse(assetSponsoredByDApp, Some(950)),
      SponsorFeeResponse(simpleAsset, Some(500)),
      SponsorFeeResponse(assetSponsoredByDApp, Some(1000))
    )

    val expectedIssueNames = Seq("asset #1", "asset #2")
    val expectedReissueResponses = Seq(
      ReissueInfoResponse(simpleAsset, isReissuable = true, 1),
      ReissueInfoResponse(assetSponsoredByDApp, isReissuable = true, 2),
      ReissueInfoResponse(simpleAsset, isReissuable = true, 3)
    )
    val expectedBurnResponses = Seq(BurnInfoResponse(simpleAsset, 3), BurnInfoResponse(assetSponsoredByDApp, 4))

    val idStateChanges2      = sender.debugStateChanges(invokeTx2._1.id).stateChanges
    val addressStateChanges2 = sender.debugStateChangesByAddress(caller, 1).head.stateChanges

    Seq(idStateChanges2, addressStateChanges2).foreach { actualStateChanges =>
      actualStateChanges.get.sponsorFees shouldBe expectedSponsorFeeResponses
      actualStateChanges.get.reissues shouldBe expectedReissueResponses
      actualStateChanges.get.burns shouldBe expectedBurnResponses
      actualStateChanges.get.issues.map(_.name) shouldBe expectedIssueNames
      actualStateChanges.get.issues.foreach { actualIssue =>
        actualIssue.quantity shouldBe 100
        actualIssue.decimals shouldBe 8
        actualIssue.isReissuable shouldBe true
      }
    }

  }

  def txInfoShouldBeEqual(info: TransactionInfo, stateChanges: DebugStateChanges)(implicit pos: Position): Unit = {
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

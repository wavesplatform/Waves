package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{issueFee, minFee, scriptBase64, setScriptFee, someAssetAmount}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class InvokeScriptErrorMsgSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val contract = pkByAddress(firstAddress)
  private val caller   = pkByAddress(secondAddress)

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    sender.transfer(sender.address, recipient = contract.stringRepr, assetId = None, amount = 5.waves, fee = minFee, waitForTx = true).id
    sender.transfer(sender.address, recipient = contract.stringRepr, assetId = None, amount = 5.waves, fee = minFee, waitForTx = true).id

    val scriptText =
      """
        |{-# STDLIB_VERSION 3 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |
        |@Callable(inv)
        |func f() = {
        | let pmt = inv.payment.extract()
        | TransferSet([ScriptTransfer(inv.caller, 1, pmt.assetId),
        | ScriptTransfer(inv.caller, 1, pmt.assetId),
        | ScriptTransfer(inv.caller, 1, pmt.assetId),
        | ScriptTransfer(inv.caller, 1, pmt.assetId),
        | ScriptTransfer(inv.caller, 1, pmt.assetId),
        | ScriptTransfer(inv.caller, 1, pmt.assetId),
        | ScriptTransfer(inv.caller, 1, pmt.assetId),
        | ScriptTransfer(inv.caller, 1, pmt.assetId),
        | ScriptTransfer(inv.caller, 1, pmt.assetId),
        | ScriptTransfer(inv.caller, 1, pmt.assetId)])
        |}
        |""".stripMargin
    val script = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    sender.setScript(contract.stringRepr, Some(script), setScriptFee, waitForTx = true).id

    sender.setScript(caller.stringRepr, Some(scriptBase64), setScriptFee, waitForTx = true).id
  }

  test("error message is informative") {
    val asset1 = sender
      .issue(
        caller.stringRepr,
        "MyAsset1",
        "Test Asset #1",
        someAssetAmount,
        0,
        reissuable = true,
        issueFee + 400000,
        2,
        Some(scriptBase64),
        waitForTx = true
      )
      .id

    assertBadRequestAndMessage(
      sender.invokeScript(
        caller.stringRepr,
        contract.stringRepr,
        Some("f"),
        payment = Seq(
          InvokeScriptTransaction.Payment(10, Asset.fromString(Some(asset1)))
        ),
        fee = 1000
      ),
      "State check failed. Reason: Transaction sent from smart account. Requires 400000 extra fee. Transaction involves 1 scripted assets." +
        " Requires 400000 extra fee. Fee for InvokeScriptTransaction (1000 in WAVES) does not exceed minimal value of 1300000 WAVES."
    )

    assertBadRequestAndMessage(
      sender.invokeScript(
        caller.stringRepr,
        contract.stringRepr,
        Some("f"),
        payment = Seq(
          InvokeScriptTransaction.Payment(10, Asset.fromString(Some(asset1)))
        ),
        fee = 1300000
      ),
      "State check failed. Reason: Fee in WAVES for InvokeScriptTransaction (1300000 in WAVES) with 12 total scripts invoked does not exceed minimal value of 5300000 WAVES."
    )
  }
}

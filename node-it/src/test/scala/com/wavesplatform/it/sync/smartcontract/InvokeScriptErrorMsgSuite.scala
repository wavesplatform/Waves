package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.api.http.ApiError.ScriptExecutionError
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class InvokeScriptErrorMsgSuite extends BaseTransactionSuite with CancelAfterFailure {
  private def contract = firstKeyPair
  private def caller   = secondKeyPair

  private lazy val contractAddress: String = contract.toAddress.toString

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    sender.transfer(sender.keyPair, recipient = contractAddress, assetId = None, amount = 5.waves, fee = minFee, waitForTx = true).id
    sender.transfer(sender.keyPair, recipient = contractAddress, assetId = None, amount = 5.waves, fee = minFee, waitForTx = true).id

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
    sender.setScript(contract, Some(script), setScriptFee, waitForTx = true).id

    sender.setScript(caller, Some(scriptBase64), setScriptFee, waitForTx = true).id
  }

  test("error message is informative") {
    val asset1 = sender
      .issue(
        caller,
        "MyAsset1",
        "Test Asset #1",
        someAssetAmount,
        0,
        fee = issueFee + 400000,
        script = Some(scriptBase64),
        waitForTx = true
      )
      .id

    assertBadRequestAndMessage(
      sender.invokeScript(
        caller,
        contractAddress,
        Some("f"),
        payment = Seq(
          InvokeScriptTransaction.Payment(10, Asset.fromString(Some(asset1)))
        ),
        fee = 1000
      ),
      "State check failed. Reason: Transaction sent from smart account. Requires 400000 extra fee. Transaction involves 1 scripted assets." +
        " Requires 400000 extra fee. Fee for InvokeScriptTransaction (1000 in WAVES) does not exceed minimal value of 1300000 WAVES."
    )

    assertApiError(
      sender
        .invokeScript(
          caller,
          contractAddress,
          Some("f"),
          payment = Seq(
            InvokeScriptTransaction.Payment(10, Asset.fromString(Some(asset1)))
          ),
          fee = 1300000
        ),
      AssertiveApiError(
        ScriptExecutionError.Id,
        "Error while executing dApp: Fee in WAVES for InvokeScriptTransaction (1300000 in WAVES) with 12 total scripts invoked does not exceed minimal value of 5300000 WAVES."
      )
    )
  }
}

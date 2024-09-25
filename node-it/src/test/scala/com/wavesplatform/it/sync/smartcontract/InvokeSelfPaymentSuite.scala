package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.api.http.ApiError.ScriptExecutionError
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.compiler.Terms.CONST_STRING
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import org.scalatest.CancelAfterFailure

class InvokeSelfPaymentSuite extends BaseTransactionSuite with CancelAfterFailure {

  private def caller = firstKeyPair
  private def dAppV4 = secondKeyPair
  private def dAppV3 = thirdKeyPair

  private var asset1: IssuedAsset = _
  private def asset1Id            = asset1.id.toString

  private lazy val dAppV3Address: String = dAppV3.toAddress.toString
  private lazy val dAppV4Address: String = dAppV4.toAddress.toString

  test("prerequisite: set contract") {
    asset1 = IssuedAsset(ByteStr.decodeBase58(sender.issue(caller, waitForTx = true).id).get)

    val sourceV4 =
      """{-# STDLIB_VERSION 4 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |
        |@Callable(inv)
        |func default() = nil
        |
        |@Callable(inv)
        |func paySelf(asset: String) = {
        |  let id = if asset == "WAVES" then unit else fromBase58String(asset)
        |  [ ScriptTransfer(this, 1, id) ]
        |}
      """.stripMargin
    val scriptV4 = ScriptCompiler.compile(sourceV4, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    sender.setScript(dAppV4, Some(scriptV4), setScriptFee)

    val sourceV3 =
      """{-# STDLIB_VERSION 3 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |
        |@Callable(inv)
        |func default() = TransferSet([])
        |
        |@Callable(inv)
        |func paySelf(asset: String) = {
        |  let id = if asset == "WAVES" then unit else fromBase58String(asset)
        |  TransferSet([ ScriptTransfer(this, 1, id) ])
        |}
      """.stripMargin
    val scriptV3 = ScriptCompiler.compile(sourceV3, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    sender.setScript(dAppV3, Some(scriptV3), setScriptFee)

    sender.massTransfer(
      caller,
      List(Transfer(dAppV4Address, 1000), Transfer(dAppV3Address, 1000)),
      smartMinFee,
      assetId = Some(asset1Id),
      waitForTx = true
    )
  }

  test("V4: can't invoke itself with payment") {
    for (
      payment <- List(
        Seq(InvokeScriptTransaction.Payment(1, Waves)),
        Seq(InvokeScriptTransaction.Payment(1, asset1)),
        Seq(InvokeScriptTransaction.Payment(1, Waves), InvokeScriptTransaction.Payment(1, asset1))
      )
    ) {
      assertApiError(
        sender.invokeScript(dAppV4, dAppV4Address, payment = payment, fee = smartMinFee + smartFee),
        AssertiveApiError(ScriptExecutionError.Id, "DApp self-payment is forbidden since V4", matchMessage = true)
      )
    }
  }

  test("V4: still can invoke itself without any payment") {
    sender.invokeScript(dAppV4, dAppV4Address, fee = smartMinFee + smartFee, waitForTx = true)
  }

  test("V4: can't send tokens to itself from a script") {
    for (
      args <- List(
        List(CONST_STRING("WAVES").explicitGet()),
        List(CONST_STRING(asset1Id).explicitGet())
      )
    ) {
      assertApiError(
        sender.invokeScript(caller, dAppV4Address, Some("paySelf"), args),
        AssertiveApiError(ScriptExecutionError.Id, "Error while executing dApp: DApp self-transfer is forbidden since V4")
      )
    }
  }

  test("V3: still can invoke itself") {
    sender.invokeScript(dAppV3, dAppV3Address, fee = smartMinFee + smartFee, waitForTx = true)
    sender.invokeScript(
      dAppV3,
      dAppV3Address,
      payment = Seq(InvokeScriptTransaction.Payment(1, Waves)),
      fee = smartMinFee + smartFee,
      waitForTx = true
    )
    sender.invokeScript(
      dAppV3,
      dAppV3Address,
      payment = Seq(InvokeScriptTransaction.Payment(1, asset1)),
      fee = smartMinFee + smartFee,
      waitForTx = true
    )
  }

  test("V3: still can pay itself") {
    sender.invokeScript(caller, dAppV3Address, Some("paySelf"), List(CONST_STRING("WAVES").explicitGet()), waitForTx = true)
    sender.invokeScript(caller, dAppV3Address, Some("paySelf"), List(CONST_STRING(asset1Id).explicitGet()), waitForTx = true)
  }

}

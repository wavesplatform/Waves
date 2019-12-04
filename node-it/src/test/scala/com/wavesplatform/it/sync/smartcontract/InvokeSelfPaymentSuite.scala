package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.api.http.ApiError.{NonPositiveAmount, StateCheckFailed}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms.CONST_STRING
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import org.scalatest.CancelAfterFailure

class InvokeSelfPaymentSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val caller = pkByAddress(firstAddress).stringRepr
  private val dAppV4 = pkByAddress(secondAddress).stringRepr
  private val dAppV3 = pkByAddress(secondAddress).stringRepr

  private var asset1: IssuedAsset = _
  private def asset1Id = asset1.id.toString

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
      List(Transfer(dAppV4, 1000), Transfer(dAppV4, 1000)),
      smartMinFee,
      Some(asset1Id),
      waitForTx = true
    )
  }

  test("V4: can't invoke itself with payment") {
    for (payment <- List(
      Seq(Payment(1, Waves)),
      Seq(Payment(1, asset1)),
      Seq(Payment(1, Waves), Payment(1, asset1))
    )) assertApiError(
      sender.invokeScript(dAppV4, dAppV4, payment = payment, fee = smartMinFee + smartFee)
    ) { error =>
      error.statusCode shouldBe 400
      error.id shouldBe StateCheckFailed.Id
      error.message should include("DApp self-payment is forbidden since V4")
    }
  }

  test("V4: still can invoke itself without any payment") {
    sender.invokeScript(dAppV4, dAppV4, fee = smartMinFee + smartFee, waitForTx = true)
  }

  test("V4: can't send tokens to itself from a script") {
    for (args <- List(
      List(CONST_STRING("WAVES").explicitGet()),
      List(CONST_STRING(asset1Id).explicitGet())
    )) assertApiError(
      sender.invokeScript(caller, dAppV4, Some("paySelf"), args)
    ) { error =>
      error.statusCode shouldBe 400
      error.id shouldBe StateCheckFailed.Id
      error.message should include("DApp self-transfer is forbidden since V4")
    }
  }

  test("V3: still can invoke itself") {
    sender.invokeScript(dAppV3, dAppV3, fee = smartMinFee + smartFee, waitForTx = true)
    sender.invokeScript(dAppV3, dAppV3, payment = Seq(Payment(1, Waves)), fee = smartMinFee + smartFee, waitForTx = true)
    sender.invokeScript(dAppV3, dAppV3, payment = Seq(Payment(1, asset1)), fee = smartMinFee + smartFee, waitForTx = true)
  }

  test("V3: still can pay itself") {
    sender.invokeScript(caller, dAppV3, Some("paySelf"), List(CONST_STRING("WAVES").explicitGet()), waitForTx = true)
    sender.invokeScript(caller, dAppV3, Some("paySelf"), List(CONST_STRING(asset1Id).explicitGet()), waitForTx = true)
  }

}

package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.compiler.Terms.CONST_STRING
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.state.IntegerDataEntry
import com.wavesplatform.test._
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class InvokeWithTransferSmartassetSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val estimator = ScriptEstimatorV2

  private def dApp      = firstKeyPair
  private def callerAcc = secondKeyPair
  private def issuerAcc = thirdKeyPair

  private val accScript = ScriptCompiler
    .compile(
      """
        |{-# STDLIB_VERSION 4 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |
        |@Callable(inv)
        |func f(assetId: String) = {
        |    if (this.getInteger("y") == 1) then {
        |     [ScriptTransfer(inv.caller, 1, fromBase58String(assetId))]
        |    } else
        |       nil
        |}
                                                """.stripMargin,
      estimator
    )
    .explicitGet()
    ._1
    .bytes()
    .base64

  private val assetScript = ScriptCompiler
    .compile(
      """
        |{-# STDLIB_VERSION 4 #-}
        |{-# CONTENT_TYPE EXPRESSION #-}
        |{-# SCRIPT_TYPE ASSET #-}
        |
        |this.issuer.getInteger("x") == 1
                                        """.stripMargin,
      estimator
    )
    .explicitGet()
    ._1
    .bytes()
    .base64

  var issuedAssetId = ""

  test("prereqisetes: issue asset and set dapp") {
    val issuerData = List(IntegerDataEntry("x", 1))
    sender.putData(issuerAcc, issuerData, 0.1.waves, waitForTx = true)

    val dAppData = List(IntegerDataEntry("y", 1))
    sender.putData(dApp, dAppData, 0.1.waves, waitForTx = true)

    issuedAssetId = sender.issue(thirdKeyPair, "some", "asset", someAssetAmount, script = Some(assetScript), waitForTx = true).id
    sender.transfer(issuerAcc, dApp.toAddress.toString, someAssetAmount, smartMinFee, Some(issuedAssetId), waitForTx = true)
    sender.setScript(firstKeyPair, Some(accScript), setScriptFee, waitForTx = true)
  }

  test("can make transfer") {
    val callerBalance = sender.assetBalance(callerAcc.toAddress.toString, issuedAssetId).balance
    val dAppBalance   = sender.assetBalance(dApp.toAddress.toString, issuedAssetId).balance

    sender
      .invokeScript(
        callerAcc,
        dApp.toAddress.toString,
        Some("f"),
        args = List(CONST_STRING(issuedAssetId).explicitGet()),
        Seq.empty,
        smartMinFee + smartFee,
        None,
        waitForTx = true
      )
      ._1
      .id

    sender.assetBalance(callerAcc.toAddress.toString, issuedAssetId).balance shouldBe callerBalance + 1
    sender.assetBalance(dApp.toAddress.toString, issuedAssetId).balance shouldBe dAppBalance - 1
  }
}

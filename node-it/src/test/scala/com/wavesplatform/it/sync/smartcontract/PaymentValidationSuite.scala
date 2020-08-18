package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.api.http.ApiError.TransactionNotAllowedByAssetScript
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.sync.{issueFee, setAssetScriptFee, setScriptFee, smartFee, smartMinFee}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer

class PaymentValidationSuite extends BaseTransactionSuite {

  test("prerequisite: set contract") {
    val dApp = firstKeyPair
    val caller = secondKeyPair
    val (wrKey, wrValue) = ("key", "value")

    val sourceV4 =
      s"""{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
        |@Callable(i)
         |func write() = {
         |  [StringEntry("$wrKey", "$wrValue")]
         |}
      """.stripMargin
    val scriptV4 = ScriptCompiler.compile(sourceV4, ScriptEstimatorV3).explicitGet()._1.bytes().base64
    sender.setScript(dApp, Some(scriptV4), setScriptFee, waitForTx = true)

    val scr = ScriptCompiler(
      s"""
         |{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE EXPRESSION #-}
         |{-# SCRIPT_TYPE ASSET #-}
         |
         |getStringValue(addressFromString("${dApp.toAddress.toString}").value(), "$wrKey") == "$wrValue"
         """.stripMargin,
      isAssetScript = true,
      ScriptEstimatorV3
    ).explicitGet()._1.bytes().base64
    val smartAssetId = sender.issue(caller, script = Some(scr), fee = issueFee + smartFee, waitForTx = true).id

    assertApiError(
      sender.invokeScript(caller, dApp.toAddress.toString, func = Some("write"), payment = Seq(Payment(1000L, IssuedAsset(ByteStr(Base58.decode(smartAssetId))))), fee = issueFee),
      AssertiveApiError(TransactionNotAllowedByAssetScript.Id, "value() called on unit value", matchMessage = true)
    )

  }
}

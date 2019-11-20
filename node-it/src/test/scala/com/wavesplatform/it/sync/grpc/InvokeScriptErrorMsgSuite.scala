package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, FUNCTION_CALL}
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.{PBTransactions, Recipient}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import io.grpc.Status.Code

class InvokeScriptErrorMsgSuite extends GrpcBaseTransactionSuite {
  private val (contract, contractAddress) = (firstAcc, firstAddress)
  private val caller   = secondAcc

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    val scriptText =
      """
        |{-# STDLIB_VERSION 3 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |
        |@Callable(inv)
        |func default() = {
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
    sender.grpc.setScript(contract, Some(script), setScriptFee, waitForTx = true)

    sender.grpc.setScript(caller, Some(scriptBase64), setScriptFee, waitForTx = true)
  }

  test("cannot invoke script without having enough fee; error message is informative") {
    val asset1 = PBTransactions.vanilla(sender.grpc.
      broadcastIssue(
        caller,
        "ScriptedAsset",
        someAssetAmount,
        decimals = 0,
        reissuable = true,
        fee = issueFee + smartFee,
        script = Some(scriptBase64),
        waitForTx = true
      )).explicitGet().id().base58

    val payments = Seq(Amount.of(ByteString.copyFrom(Base58.decode(asset1)), 10))
    assertGrpcError(
      sender.grpc.broadcastInvokeScript(
        caller,
        Recipient().withAddress(contractAddress),
        None,
        payments = payments,
        fee = 1000
      ),
      "Transaction sent from smart account. Requires 400000 extra fee. Transaction involves 1 scripted assets",
      Code.INVALID_ARGUMENT
    )

    assertGrpcError(
      sender.grpc.broadcastInvokeScript(
        caller,
        Recipient().withAddress(contractAddress),
        None,
        payments = payments,
        fee = 1300000
      ),
      "State check failed. Reason: Fee in WAVES for InvokeScriptTransaction (1300000 in WAVES) with 12 total scripts invoked does not exceed minimal value of 5300000 WAVES.",
      Code.INVALID_ARGUMENT
    )
  }

}

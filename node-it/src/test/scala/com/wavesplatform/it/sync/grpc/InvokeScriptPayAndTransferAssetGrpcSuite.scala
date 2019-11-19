package com.wavesplatform.it.sync.grpc

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.sync._
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class InvokeScriptPayAndTransferAssetGrpcSuite extends GrpcBaseTransactionSuite {
  private val estimator = ScriptEstimatorV2

  private val (dApp, dAppAddress)     = (firstAcc, firstAddress)
  private val (caller, callerAddress)   = (secondAcc, secondAddress)
  private val (receiver, receiverAddress) = (thirdAcc, thirdAddress)

  var dAppInitBalance: Long     = 0
  var callerInitBalance: Long   = 0
  var receiverInitBalance: Long = 0
  val assetQuantity: Long       = 15
  var assetId: String           = ""
  var smartAssetId: String      = ""
  var rejAssetId: String        = ""

  test("issue and transfer asset") {
    assetId = PBTransactions.vanilla(
      sender.grpc.broadcastIssue(caller, "Asset", assetQuantity, 2, reissuable = true, fee = issueFee, waitForTx = true)
    ).explicitGet().id().base58

    val script = Some(ScriptCompiler.compile("true", estimator).explicitGet()._1.bytes.value.base64)
    smartAssetId = PBTransactions.vanilla(
      sender.grpc.broadcastIssue(caller, "Smart", assetQuantity, 2, reissuable = true, fee = issueFee, script = script, waitForTx = true)
    ).explicitGet().id().base58

    val scriptText  = "match tx {case t:TransferTransaction => false case _ => true}"
    val smartScript = Some(ScriptCompiler.compile(scriptText, estimator).explicitGet()._1.bytes.value.base64)
    rejAssetId = PBTransactions.vanilla(
      sender.grpc.broadcastIssue(caller, "Reject", assetQuantity, 2, reissuable = true, fee = issueFee, script = smartScript, waitForTx = true)
    ).explicitGet().id().base58
  }

}

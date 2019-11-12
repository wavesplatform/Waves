package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{minFee, setScriptFee, transferAmount}
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransactionV2
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.protobuf.transaction.{PBTransactions, Script => PBScript}

class SetScriptTransactionGrpcSuite extends GrpcBaseTransactionSuite {

  test("set a multisig script to account") {
    val scriptText = s"""
        match tx {
          case t: Transaction => {
            let A = base58'${ByteStr(secondAcc.publicKey)}'
            let B = base58'${ByteStr(thirdAcc.publicKey)}'
            let AC = sigVerify(tx.bodyBytes,tx.proofs[0],A)
            let BC = sigVerify(tx.bodyBytes,tx.proofs[1],B)
            AC && BC
          }
          case _ => false
        }
      """.stripMargin

    val script      = ScriptCompiler(scriptText, isAssetScript = false, ScriptEstimatorV2).explicitGet()._1//.bytes().base64
    val scriptComplexity = Script.estimate(Script.fromBase64String(script.bytes().base64).explicitGet(), ScriptEstimatorV2).explicitGet()
    val setScriptTx = sender.grpc.setScript(firstAcc, Some(script.bytes().base64), setScriptFee, waitForTx = true)
    val setScriptTxId = PBTransactions.vanilla(setScriptTx).explicitGet().id().base58

    val scriptInfo = sender.grpc.scriptInfo(firstAddress)

    scriptInfo.scriptBytes shouldBe ByteString.copyFrom(Base64.decode(script.bytes().base64))
    scriptInfo.scriptText shouldBe script.expr.toString
    scriptInfo.complexity shouldBe scriptComplexity

    sender.grpc.getTransaction(setScriptTxId).getTransaction.getSetScript.script.get shouldBe PBScript.of(ByteString.copyFrom(Base64.decode(script.bytes().base64)))
  }

}

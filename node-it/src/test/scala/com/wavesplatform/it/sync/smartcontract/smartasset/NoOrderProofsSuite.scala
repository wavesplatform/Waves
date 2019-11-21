package com.wavesplatform.it.sync.smartcontract.smartasset

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{someAssetAmount, _}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.BurnTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.{Attachment, TransferTransaction}

import scala.concurrent.duration._

class NoOrderProofsSuite extends BaseTransactionSuite {
  val estimator = ScriptEstimatorV2
  test("try to use Order in asset scripts") {
    try {
      sender.issue(
        firstAddress,
        "assetWProofs",
        "Test coin for assetWProofs test",
        someAssetAmount,
        0,
        reissuable = true,
        issueFee,
        2,
        script = Some(
          ScriptCompiler(
            s"""
              |match tx {
              |  case o: Order => true
              |  case _ => false
              |}""".stripMargin,
            isAssetScript = true,
            estimator
          ).explicitGet()._1.bytes.value.base64
        )
      )

      fail("ScriptCompiler didn't throw expected error")
    } catch {
      case ex: java.lang.Exception => ex.getMessage should include("Compilation failed: Matching not exhaustive")
      case _: Throwable            => fail("ScriptCompiler works incorrect for orders with smart assets")
    }
  }

  test("try to use proofs in assets script") {
    val errProofMsg = "Reason: Proof doesn't validate as signature"
    val assetWProofs = sender
      .issue(
        firstAddress,
        "assetWProofs",
        "Test coin for assetWProofs test",
        someAssetAmount,
        0,
        reissuable = true,
        issueFee,
        2,
        script = Some(
          ScriptCompiler(
            s"""
                let proof = base58'assetWProofs'
                match tx {
                  case tx: SetAssetScriptTransaction | TransferTransaction | ReissueTransaction | BurnTransaction => tx.proofs[0] == proof
                  case _ => false
                }""".stripMargin,
            false,
            estimator
          ).explicitGet()._1.bytes.value.base64
        ),
        waitForTx = true
      )
      .id

    val incorrectTrTx = TransferTransaction(
      2.toByte,
      pkByAddress(firstAddress),
      pkByAddress(thirdAddress),
      IssuedAsset(ByteStr.decodeBase58(assetWProofs).get),
      1,
      Waves,
      smartMinFee,
      Attachment.Empty,
      System.currentTimeMillis + 10.minutes.toMillis,
      Proofs(Seq(ByteStr("assetWProofs".getBytes("UTF-8"))))
    )

    assertBadRequestAndMessage(
      sender.signedBroadcast(incorrectTrTx.json()),
      errProofMsg
    )

    val incorrectBrTx = BurnTransaction
      .create(
        2.toByte,
        pkByAddress(firstAddress),
        IssuedAsset(ByteStr.decodeBase58(assetWProofs).get),
        1,
        smartMinFee,
        System.currentTimeMillis + 10.minutes.toMillis,
        Proofs(Seq(ByteStr("assetWProofs".getBytes("UTF-8"))))
      )
      .right
      .get

    assertBadRequestAndMessage(
      sender.signedBroadcast(incorrectBrTx.json()),
      errProofMsg
    )
  }

}

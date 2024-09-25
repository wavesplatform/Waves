package com.wavesplatform.it.sync.smartcontract.smartasset

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.sync.{someAssetAmount, *}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.{Proofs, TxPositiveAmount}
import com.wavesplatform.transaction.assets.BurnTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransaction

import scala.concurrent.duration.*

class NoOrderProofsSuite extends BaseTransactionSuite {
  val estimator = ScriptEstimatorV2
  test("try to use Order in asset scripts") {
    try {
      sender.issue(
        firstKeyPair,
        "assetWProofs",
        "Test coin for assetWProofs test",
        someAssetAmount,
        0,
        reissuable = true,
        issueFee,
        2: Byte,
        script = Some(
          TestCompiler.DefaultVersion
            .compileAsset(
              s"""
                 |match tx {
                 |  case _: Order => true
                 |  case _ => false
                 |}""".stripMargin
            )
            .bytes()
            .base64
        )
      )

      fail("ScriptCompiler didn't throw expected error")
    } catch {
      case ex: java.lang.Exception => ex.getMessage should include("Compilation failed: [Matching not exhaustive")
      case _: Throwable            => fail("ScriptCompiler works incorrect for orders with smart assets")
    }
  }

  test("try to use proofs in assets script") {
    val errProofMsg = "Reason: Proof doesn't validate as signature"
    val assetWProofs = sender
      .issue(
        firstKeyPair,
        "assetWProofs",
        "Test coin for assetWProofs test",
        someAssetAmount,
        0,
        reissuable = true,
        issueFee,
        2: Byte,
        script = Some(
          ScriptCompiler
            .compile(
              s"""
                let proof = base58'assetWProofs'
                match tx {
                  case _: SetAssetScriptTransaction | TransferTransaction | ReissueTransaction | BurnTransaction => tx.proofs[0] == proof
                  case _ => false
                }""".stripMargin,
              estimator
            )
            .explicitGet()
            ._1
            .bytes()
            .base64
        ),
        waitForTx = true
      )
      .id

    val incorrectTrTx = TransferTransaction(
      2.toByte,
      firstKeyPair.publicKey,
      thirdKeyPair.toAddress,
      IssuedAsset(ByteStr.decodeBase58(assetWProofs).get),
      TxPositiveAmount.unsafeFrom(1),
      Waves,
      TxPositiveAmount.unsafeFrom(smartMinFee),
      ByteStr.empty,
      System.currentTimeMillis + 10.minutes.toMillis,
      Proofs(Seq(ByteStr("assetWProofs".getBytes("UTF-8")))),
      AddressScheme.current.chainId
    )

    assertBadRequestAndMessage(
      sender.signedBroadcast(incorrectTrTx.json()),
      errProofMsg
    )

    val incorrectBrTx = BurnTransaction
      .create(
        2.toByte,
        firstKeyPair.publicKey,
        IssuedAsset(ByteStr.decodeBase58(assetWProofs).get),
        1,
        smartMinFee,
        System.currentTimeMillis + 10.minutes.toMillis,
        Proofs(Seq(ByteStr("assetWProofs".getBytes("UTF-8")))),
        AddressScheme.current.chainId
      )
      .explicitGet()

    assertBadRequestAndMessage(
      sender.signedBroadcast(incorrectBrTx.json()),
      errProofMsg
    )
  }

}

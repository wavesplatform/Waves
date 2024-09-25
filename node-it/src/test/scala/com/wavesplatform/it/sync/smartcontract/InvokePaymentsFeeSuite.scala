package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.smartMinFee
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class InvokePaymentsFeeSuite extends BaseTransactionSuite {

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(
        _.preactivatedFeatures(
          (BlockchainFeatures.Ride4DApps.id, 0),
          (BlockchainFeatures.BlockV5.id, 0),
          (BlockchainFeatures.SynchronousCalls.id, 0)
        )
      )
      .withDefault(1)
      .buildNonConflicting()

  private lazy val (caller, callerAddress) = (firstKeyPair, firstAddress)
  private lazy val (dApp, dAppAddress)     = (secondKeyPair, secondAddress)

  val verifier: String = {
    val script = s"""
                    | {-# STDLIB_VERSION 4        #-}
                    | {-# SCRIPT_TYPE ASSET       #-}
                    | {-# CONTENT_TYPE EXPRESSION #-}
                    |
                    | !(sigVerify_32Kb(base58'', base58'', base58'') ||
                    |   sigVerify_32Kb(base58'', base58'', base58'') ||
                    |   sigVerify_32Kb(base58'', base58'', base58'')
                    |  )
                    |
                    """.stripMargin
    ScriptCompiler.compile(script, ScriptEstimatorV3.latest).explicitGet()._1.bytes().base64
  }

  private def dApp(assetId: String): String =
    ScriptCompiler
      .compile(
        s"""
           | {-# STDLIB_VERSION 4       #-}
           | {-# CONTENT_TYPE   DAPP    #-}
           | {-# SCRIPT_TYPE    ACCOUNT #-}
           |
           | @Callable(i)
           | func default() =
           |   [
           |     ScriptTransfer(i.caller, 1, base58'$assetId'),
           |     Burn(base58'$assetId', 1),
           |     Reissue(base58'$assetId', 1, false)
           |   ]
       """.stripMargin,
        ScriptEstimatorV3.latest
      )
      .explicitGet()
      ._1
      .bytes()
      .base64

  test(s"fee for asset scripts is not required after activation ${BlockchainFeatures.SynchronousCalls}") {
    val assetId = sender.issue(dApp, script = Some(verifier), waitForTx = true).id
    val asset   = IssuedAsset(ByteStr.decodeBase58(assetId).get)

    sender.transfer(dApp, callerAddress, 100, assetId = Some(assetId), fee = smartMinFee, waitForTx = true)
    sender.setScript(dApp, Some(dApp(assetId)), waitForTx = true)

    val payments = Seq(Payment(1, asset), Payment(1, asset))
    val invokeId = sender.invokeScript(caller, dAppAddress, payment = payments, waitForTx = true)._1.id
    sender.transactionStatus(invokeId).status shouldBe "confirmed"
  }
}

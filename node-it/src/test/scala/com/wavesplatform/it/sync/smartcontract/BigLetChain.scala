package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.RideV6
import com.wavesplatform.it.BaseFunSuite
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.sync.*
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransaction
import org.scalatest.CancelAfterFailure

class BigLetChain extends BaseFunSuite with CancelAfterFailure {
  import com.wavesplatform.it.NodeConfigs.*
  override protected def nodeConfigs: Seq[Config] = Seq(
    Miners(5).quorum(0).preactivatedFeatures(BlockchainFeatures.BlockV5, RideV6)
  )

  test("big let assignment chain") {
    val count = 280
    val scriptText =
      s"""
         | {-# STDLIB_VERSION 3    #-}
         | {-# CONTENT_TYPE   DAPP #-}
         |
         | @Verifier(tx)
         | func verify() = {
         |   let a0 = 1
         |   ${1 to count map (i => s"let a$i = a${i - 1}") mkString "\n"}
         |   a$count == a$count
         | }
       """.stripMargin

    val compiledScript = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1

    val pkNewAddress = sender.createKeyPair()

    sender.transfer(sender.keyPair, pkNewAddress.toAddress.toString, 10.waves, minFee, waitForTx = true)

    val scriptSet          = SetScriptTransaction.create(1.toByte, pkNewAddress.publicKey, Some(compiledScript), setScriptFee, System.currentTimeMillis(), Proofs.empty).map(_.signWith(pkNewAddress.privateKey))
    val scriptSetBroadcast = sender.signedBroadcast(scriptSet.explicitGet().json())
    nodes.waitForHeightAriseAndTxPresent(scriptSetBroadcast.id)

    val transfer = TransferTransaction.create(
      2.toByte,
      pkNewAddress.publicKey,
      pkNewAddress.toAddress,
      Waves,
      1.waves,
      Waves,
      smartMinFee,
      ByteStr.empty,
      System.currentTimeMillis(),
      Proofs.empty
    ).map(_.signWith(pkNewAddress.privateKey))
    val transferBroadcast = sender.signedBroadcast(transfer.explicitGet().json())
    nodes.waitForHeightAriseAndTxPresent(transferBroadcast.id)
  }
}

package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransactionV2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import org.scalatest.CancelAfterFailure
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2

class BigLetChain extends BaseTransactionSuite with CancelAfterFailure {
  test("big let assignment chain") {
    val count = 550
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

    val newAddress   = sender.createAddress()
    val acc0         = pkByAddress(firstAddress)
    val pkNewAddress = pkByAddress(newAddress)

    sender.transfer(acc0.stringRepr, newAddress, 10.waves, minFee, waitForTx = true)

    val scriptSet = SetScriptTransaction.selfSigned(
      pkNewAddress,
      Some(compiledScript),
      setScriptFee,
      System.currentTimeMillis()
    )
    val scriptSetBroadcast = sender.signedBroadcast(scriptSet.explicitGet().json.value)
    nodes.waitForHeightAriseAndTxPresent(scriptSetBroadcast.id)

    val transfer = TransferTransactionV2.selfSigned(
      Waves,
      pkNewAddress,
      pkNewAddress,
      1.waves,
      System.currentTimeMillis(),
      Waves,
      smartMinFee,
      Array()
    )
    val transferBroadcast = sender.signedBroadcast(transfer.explicitGet().json.value)
    nodes.waitForHeightAriseAndTxPresent(transferBroadcast.id)
  }
}

package com.wavesplatform.state.diffs.ci

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures._
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import com.wavesplatform.{TestTime, TransactionGenBase}

class OverheadCallableCallTest extends PropSpec with WithDomain with TransactionGenBase {
  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private val body = {
    val n = 65
    s"""
       | func f0() = true
       | ${(0 until n).map(i => s"func f${i + 1}() = if (f$i()) then f$i() else f$i()").mkString("\n")}
       | f$n()
       """.stripMargin
  }

  private val dApp1Script: Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   strict r = $body
         |   []
         | }
       """.stripMargin
    )

  private val scenario =
    for {
      invoker <- accountGen
      dApp1   <- accountGen
      fee     <- ciFee()
      gTx1     = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx2     = GenesisTransaction.create(dApp1.toAddress, ENOUGH_AMT, ts).explicitGet()
      ssTx1    = SetScriptTransaction.selfSigned(1.toByte, dApp1, Some(dApp1Script), fee, ts).explicitGet()
      invokeTx = () => InvokeScriptTransaction.selfSigned(TxVersion.V3, invoker, dApp1.toAddress, None, Nil, fee, Waves, ts).explicitGet()
    } yield (Seq(gTx1, gTx2, ssTx1), invokeTx)

  private val settings =
    TestFunctionalitySettings
      .withFeatures(BlockV5, SynchronousCalls)
      .copy(estimationOverflowFixHeight = 999, estimatorSumOverflowFixHeight = 3)

  property("overhead callable call should be safe both before and after fix") {
      val (preparingTxs, invoke) = scenario.sample.get
      withDomain(domainSettingsWithFS(settings)) { d =>
        d.appendBlock(preparingTxs: _*)
        (the[Exception] thrownBy d.appendBlock(invoke())).getMessage should include("Evaluation was uncompleted with unused complexity = 0")
        d.appendBlock()
        (the[Exception] thrownBy d.appendBlock(invoke())).getMessage should include("Evaluation was uncompleted with unused complexity = 0")
      }
  }
}

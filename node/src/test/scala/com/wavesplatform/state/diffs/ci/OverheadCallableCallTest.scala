package com.wavesplatform.state.diffs.ci

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures.*
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers

class OverheadCallableCallTest extends PropSpec with WithDomain {

  private val body = {
    val n = 65
    s"""
       | func f0() = true
       | ${(0 until n).map(i => s"func f${i + 1}() = if (f$i()) then f$i() else f$i()").mkString("\n")}
       | f$n()
       """.stripMargin
  }

  private val dAppScript: Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   strict r = $body
         |   []
         | }
       """.stripMargin
    )

  private val settings =
    TestFunctionalitySettings
      .withFeatures(BlockV5, SynchronousCalls)
      .copy(estimationOverflowFixHeight = 999, estimatorSumOverflowFixHeight = 4)

  property("overhead callable call should be safe both before and after fix") {
    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)

    val balances = AddrWithBalance.enoughBalances(invoker, dApp)

    val setScript = TxHelpers.setScript(dApp, dAppScript)
    val invoke1   = TxHelpers.invoke(dApp.toAddress, func = None, invoker = invoker)
    val invoke2   = TxHelpers.invoke(dApp.toAddress, func = None, invoker = invoker)

    withDomain(domainSettingsWithFS(settings), balances) { d =>
      d.appendBlock(setScript)
      d.appendBlockE(invoke1) should produce("Evaluation was uncompleted with unused complexity = 0")
      d.appendBlock()
      d.appendBlockE(invoke2) should produce("Evaluation was uncompleted with unused complexity = 0")
    }
  }
}

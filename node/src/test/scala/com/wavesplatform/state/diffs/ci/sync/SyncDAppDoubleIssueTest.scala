package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.account.Address
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures._
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.test._
import com.wavesplatform.transaction.TxHelpers

class SyncDAppDoubleIssueTest extends PropSpec with WithDomain {

  private def sigVerify(c: Boolean) =
    s""" strict c = ${if (c) (1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ") else "true"} """

  private def dApp1Script(dApp2: Address, bigComplexity: Boolean): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |    ${sigVerify(bigComplexity)}
         |    strict r = Address(base58'$dApp2').invoke("default", [], [])
         |    [
         |      Issue("name", "description", 1000, 4, true, unit, 0)
         |    ]
         | }
       """.stripMargin
    )

  private def dApp2Script(bigComplexity: Boolean): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   ${sigVerify(bigComplexity)}
         |   [
         |     Issue("name", "description", 1000, 4, true, unit, 0)
         |   ]
         | }
       """.stripMargin
    )

  private val settings =
    TestFunctionalitySettings
      .withFeatures(BlockV5, SynchronousCalls)
      .copy(syncDAppCheckTransfersHeight = 3)

  property("issue the same asset via 2 dApps") {
    for {
      bigComplexityDApp1 <- Seq(false, true)
      bigComplexityDApp2 <- Seq(false, true)
    } {
      val invoker = TxHelpers.signer(0)
      val dApp1   = TxHelpers.signer(1)
      val dApp2   = TxHelpers.signer(2)

      val genesis = Seq(
        TxHelpers.genesis(invoker.toAddress),
        TxHelpers.genesis(dApp1.toAddress),
        TxHelpers.genesis(dApp2.toAddress)
      )
      val setScript1 = TxHelpers.setScript(dApp1, dApp1Script(dApp2.toAddress, bigComplexityDApp1))
      val setScript2 = TxHelpers.setScript(dApp2, dApp2Script(bigComplexityDApp2))

      val preparingTxs = genesis ++ Seq(setScript1, setScript2)

      val invokeFee = 200500000.waves
      val invoke1   = TxHelpers.invoke(dApp1.toAddress, func = None, invoker = invoker, fee = invokeFee)
      val invoke2   = TxHelpers.invoke(dApp1.toAddress, func = None, invoker = invoker, fee = invokeFee)

      withDomain(domainSettingsWithFS(settings)) { d =>
        d.appendBlock(preparingTxs: _*)

        d.appendBlock(invoke1)
        d.liquidDiff.errorMessage(invoke1.txId).get.text should include("already issued")

        d.appendBlockE(invoke2) should produce("already issued")
      }
    }
  }
}

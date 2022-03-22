package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.account.Address
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures.*
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.test.*
import com.wavesplatform.transaction.{Asset, TxHelpers}
import com.wavesplatform.transaction.Asset.IssuedAsset

class SyncDAppNegativeTransferTest extends PropSpec with WithDomain {

  private def sigVerify(c: Boolean) =
    s""" strict c = ${if (c) (1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ") else "true"} """

  private def dApp1Script(dApp2: Address, bigComplexity: Boolean): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |    ${sigVerify(bigComplexity)}
         |    strict r = Address(base58'$dApp2').invoke("default", [], [])
         |    []
         | }
       """.stripMargin
    )

  private def dApp2Script(asset: Asset, bigComplexity: Boolean): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   ${sigVerify(bigComplexity)}
         |   [
         |     ScriptTransfer(i.caller, -1, base58'$asset')
         |   ]
         | }
       """.stripMargin
    )

  private val settings =
    TestFunctionalitySettings
      .withFeatures(BlockV5, SynchronousCalls)

  property("negative transfer amount") {
    for {
      bigComplexityDApp1 <- Seq(false, true)
      bigComplexityDApp2 <- Seq(false, true)
    } {
      val invoker = TxHelpers.signer(0)
      val dApp1   = TxHelpers.signer(1)
      val dApp2   = TxHelpers.signer(2)

      val balances = AddrWithBalance.enoughBalances(invoker, dApp1, dApp2)

      val issue      = TxHelpers.issue(dApp2, 100)
      val asset      = IssuedAsset(issue.id.value())
      val setScript1 = TxHelpers.setScript(dApp1, dApp1Script(dApp2.toAddress, bigComplexityDApp1))
      val setScript2 = TxHelpers.setScript(dApp2, dApp2Script(asset, bigComplexityDApp2))

      val preparingTxs = Seq(issue, setScript1, setScript2)

      val invoke = TxHelpers.invoke(dApp1.toAddress, func = None, invoker = invoker)

      withDomain(domainSettingsWithFS(settings), balances) { d =>
        d.appendBlock(preparingTxs*)

        if (!bigComplexityDApp1 && !bigComplexityDApp2) {
          d.appendAndCatchError(invoke).toString should include("Negative transfer amount")
        } else {
          d.appendAndAssertFailed(invoke)
        }
      }
    }
  }
}

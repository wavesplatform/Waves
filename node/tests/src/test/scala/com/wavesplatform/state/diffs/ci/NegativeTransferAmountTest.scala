package com.wavesplatform.state.diffs.ci

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures.*
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{Asset, TxHelpers}

class NegativeTransferAmountTest extends PropSpec with WithDomain {

  private def sigVerify(c: Boolean) =
    s""" strict c = ${if (c) (1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ") else "true"} """

  private def dAppScript(asset: Asset, bigComplexity: Boolean): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |    ${sigVerify(bigComplexity)}
         |    [
         |      ScriptTransfer(i.caller, -1, base58'$asset')
         |    ]
         | }
       """.stripMargin
    )

  private val settings =
    TestFunctionalitySettings
      .withFeatures(BlockV5, SynchronousCalls, RideV6)

  property("negative transfer amount") {
    for (bigComplexity <- Seq(false, true)) {
      val invoker = TxHelpers.signer(0)
      val dApp    = TxHelpers.signer(1)

      val balances = AddrWithBalance.enoughBalances(invoker, dApp)

      val issue     = TxHelpers.issue(dApp, 100)
      val setScript = TxHelpers.setScript(dApp, dAppScript(IssuedAsset(issue.id.value()), bigComplexity))

      val preparingTxs = Seq(issue, setScript)

      val invoke = TxHelpers.invoke(dApp.toAddress, func = None, invoker = invoker)

      withDomain(domainSettingsWithFS(settings), balances) { d =>
        d.appendBlock(preparingTxs*)

        if (!bigComplexity) {
          d.appendAndCatchError(invoke).toString should include("Negative transfer amount")
        } else {
          d.appendAndAssertFailed(invoke)
        }
      }
    }
  }
}

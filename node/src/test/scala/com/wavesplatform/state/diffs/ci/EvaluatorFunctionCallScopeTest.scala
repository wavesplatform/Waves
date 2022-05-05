package com.wavesplatform.state.diffs.ci

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures._
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.test._
import com.wavesplatform.transaction.TxHelpers

class EvaluatorFunctionCallScopeTest extends PropSpec with WithDomain {

  private val dAppScript: Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   let a = 4
         |   func g(b: Int) = a
         |   func f(a: Int) = g(a)
         |   let r = f(1)
         |   [
         |     IntegerEntry("key", r)
         |   ]
         | }
       """.stripMargin
    )

  private val settings =
    TestFunctionalitySettings
      .withFeatures(BlockV5, SynchronousCalls)
      .copy(estimatorSumOverflowFixHeight = 4)

  property("arg of the first function should NOT overlap var accessed from body of the second function AFTER fix") {
    val invoker  = TxHelpers.signer(0)
    val dApp     = TxHelpers.signer(1)
    val balances = AddrWithBalance.enoughBalances(invoker, dApp)

    val setScript = TxHelpers.setScript(dApp, dAppScript)
    val invoke    = () => TxHelpers.invoke(dApp.toAddress, func = None, invoker = invoker)

    withDomain(domainSettingsWithFS(settings), balances) { d =>
      d.appendBlock(setScript)

      d.appendBlock(invoke())
      d.blockchain.accountData(dApp.toAddress, "key").get.value shouldBe 1

      d.appendBlock(invoke())
      d.blockchain.accountData(dApp.toAddress, "key").get.value shouldBe 4
    }
  }
}

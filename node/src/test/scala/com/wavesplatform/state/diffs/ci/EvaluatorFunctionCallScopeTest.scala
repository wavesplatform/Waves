package com.wavesplatform.state.diffs.ci

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures.*
import com.wavesplatform.lang.directives.values.{V5, V6}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers

class EvaluatorFunctionCallScopeTest extends PropSpec with WithDomain {

  private val settings =
    TestFunctionalitySettings
      .withFeatures(BlockV5, SynchronousCalls)
      .copy(estimatorSumOverflowFixHeight = 4)

  property("arg of the first function should NOT overlap var accessed from body of the second function AFTER fix") {
    val invoker  = TxHelpers.signer(0)
    val dApp     = TxHelpers.signer(1)
    val balances = AddrWithBalance.enoughBalances(invoker, dApp)

    val dAppScript: Script =
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

  property("evaluator should correctly process scopes for global declarations") {
    val dAppScript =
      TestCompiler(V6).compileContract(
        s"""
           |{-# STDLIB_VERSION 6 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |
           |let a = {
           |  func bar(i: Int) = i
           |  bar(1)
           |}
           |let b = {
           |  let c = {
           |    let d = {
           |      func bar(i: Int) = i + 1
           |      bar(a)
           |    }
           |
           |    func bar(i: Int) = i * 2
           |    bar(d)
           |  }
           |
           |  func bar(i: Int) = i + 1
           |  bar(c)
           |}
           |
           |@Callable(i)
           |func default() = {
           | [
           |    IntegerEntry("key", b)
           | ]
           |}
           |""".stripMargin
      )
    val invoker   = TxHelpers.signer(0)
    val dApp      = TxHelpers.signer(1)
    val balances  = AddrWithBalance.enoughBalances(invoker, dApp)
    val setScript = TxHelpers.setScript(dApp, dAppScript)
    val invoke    = () => TxHelpers.invoke(dApp.toAddress, func = None, invoker = invoker)

    withDomain(DomainPresets.TransactionStateSnapshot, balances) { d =>
      d.appendBlock(setScript)
      d.appendBlock(invoke())
      d.blockchain.accountData(dApp.toAddress, "key").get.value shouldBe 5
    }
  }
}

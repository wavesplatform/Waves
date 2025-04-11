package com.wavesplatform.state.diffs.ci

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures.*
import com.wavesplatform.lang.directives.values.{V5, V6, V7, V8}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_STRING, CONST_LONG}
import com.wavesplatform.common.utils.EitherExt2.*
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

  property("overhead with recursive callable call should be safe both before and after fix") {
    val invoker = TxHelpers.signer(0)
    val dApp    = TxHelpers.signer(1)

    val balances = AddrWithBalance.enoughBalances(invoker, dApp)

    val dAppScript =
      TestCompiler(V5).compileContract(
        s"""
           |  {-# STDLIB_VERSION 5 #-}
           |  {-# CONTENT_TYPE DAPP #-}
           |  {-# SCRIPT_TYPE ACCOUNT #-}
           |
           |  # empty, but has 1 complexity after RideV6 activation
           |  func nonInvokeLightCall() = nil
           |
           |  # has 2 complexity
           |  func nonInvokeCall() = {
           |    strict t = true
           |    strict f = false
           |    []
           |  }
           |
           |  @Callable(i)
           |  func entrypoint() = {
           |    strict lc = invoke(this, "lightCall", [], []) # 77 complexity
           |    strict nilc = nonInvokeLightCall() # 2 complexity
           |    strict nlc = invoke(this, "nonLightCall", [], []) # 77 complexity
           |    strict nic = nonInvokeCall() # 3 complexity
           |    strict c = invoke(this, "call", [], []) # 78 complexity
           |    []
           |  }
           |
           |  # empty, but has 1 complexity after RideV6 activation
           |  @Callable(i)
           |  func lightCall() = nil
           |
           |  # has 1 complexity
           |  @Callable(i)
           |  func nonLightCall() = {
           |    strict caller = i.caller
           |    []
           |  }
           |
           |  # has 2 complexity
           |  @Callable(i)
           |  func call() = {
           |    strict c = i.caller
           |    strict cc = i.caller
           |    []
           |  }
           |
           |  # has 2 complexity
           |  @Callable(i)
           |  func fail() = {
           |    throw()
           |  }
           |
           |  # has 1 complexity
           |  @Callable(i)
           |  func failMsg(msg: String) = {
           |    throw(msg)
           |  }
         """.stripMargin
      )

    val setScript = TxHelpers.setScript(dApp, dAppScript)
    val invoke1 =
      TxHelpers.invoke(
        dApp.toAddress,
        invoker = invoker,
        func = Some("entrypoint"),
        args = Seq()
      )
    val invoke2 =
      TxHelpers.invoke(
        dApp.toAddress,
        invoker = invoker,
        func = Some("lightCall"),
        args = Seq()
      )

    val invoke3 =
      TxHelpers.invoke(
        dApp.toAddress,
        invoker = invoker,
        func = Some("nonLightCall"),
        args = Seq()
      )

    val invoke4 =
      TxHelpers.invoke(
        dApp.toAddress,
        invoker = invoker,
        func = Some("call"),
        args = Seq()
      )

    val invoke5 =
      TxHelpers.invoke(
        dApp.toAddress,
        invoker = invoker,
        func = Some("fail"),
        args = Seq()
      )

    val invoke6 =
      TxHelpers.invoke(
        dApp.toAddress,
        invoker = invoker,
        func = Some("failMsg"),
        args = Seq(CONST_STRING("!expected failure!").explicitGet())
      )

    val feats = Seq(
      SmallerMinimalGeneratingBalance -> 0,
      NG                              -> 0,
      MassTransfer                    -> 0,
      SmartAccounts                   -> 0,
      DataTransaction                 -> 0,
      BurnAnyTokens                   -> 0,
      FeeSponsorship                  -> 0,
      FairPoS                         -> 0,
      SmartAssets                     -> 0,
      SmartAccountTrading             -> 0,
      Ride4DApps                      -> 0,
      BlockReward                     -> 0,
      BlockV5                         -> 0,
      SynchronousCalls                -> 0,
      ConsensusImprovements           -> 0,
      BlockRewardDistribution         -> 0,
      CappedReward                    -> 0,
      CeaseXtnBuyback                 -> 0,
      LightNode                       -> 0
    )

    println("========Without RideV6=========")

    val featsWithoutRideV6 = feats ++ Seq(RideV6 -> 100)
    val setsDisabledRideV6 = TestFunctionalitySettings.withFeaturesByHeight(featsWithoutRideV6*)
    withDomain(domainSettingsWithFS(setsDisabledRideV6), balances) { d =>
      d.appendBlock(setScript)
      d.appendBlockE(invoke1) should beRight
//      d.appendBlockE(invoke2) should produce("Evaluation was uncompleted with unused complexity = 0")
      d.appendBlockE(invoke2) should beRight
      d.appendBlockE(invoke3) should beRight
      d.appendBlockE(invoke4) should beRight
      d.appendBlockE(invoke5) should beLeft
      d.appendBlockE(invoke6) should beLeft
    }

    println("========With RideV6=========")

    val featsWithRideV6   = feats ++ Seq(RideV6 -> 0)
    val setsEnabledRideV6 = TestFunctionalitySettings.withFeaturesByHeight(featsWithRideV6*)
    withDomain(domainSettingsWithFS(setsEnabledRideV6), balances) { d =>
      d.appendBlock(setScript)
      d.appendBlockE(invoke1) should beRight
      d.appendBlockE(invoke2) should beRight
      d.appendBlockE(invoke3) should beRight
      d.appendBlockE(invoke4) should beRight
      d.appendBlockE(invoke5) should beLeft
      d.appendBlockE(invoke6) should beLeft
    }
  }
}

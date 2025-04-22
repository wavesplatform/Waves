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
  property("TestInvokeActionsCountRestrictionsV6ToV5WithBlockRewardDistributionFailed") {
    val invoker = TxHelpers.signer(0)
    val dApp1   = TxHelpers.signer(1)
    val dApp2   = TxHelpers.signer(2)
    val dApp3   = TxHelpers.signer(3)

    val balances = AddrWithBalance.enoughBalances(invoker, dApp1, dApp2, dApp3)

    val dApp1Script =
      TestCompiler(V6).compileContract(
        s"""
           | {-# STDLIB_VERSION 6 #-}
           | {-# CONTENT_TYPE DAPP #-}
           | {-# SCRIPT_TYPE ACCOUNT #-}
           |
           | let dApp2 = Address(base58'${dApp2.toAddress.toString}') # V6 script produces 10 transfer actions
           | let dApp3 = Address(base58'${dApp3.toAddress.toString}') # V5 script produces no actions
           |
           | @Callable(i)
           | func call() = {
           |	 strict a1 = invoke(dApp2,  "call", [], [])
           |	 strict b1 = invoke(dApp3,  "call", [], [])
           |	 strict a2 = invoke(dApp2,  "call", [], [])
           |	 strict b2 = invoke(dApp3,  "call", [], [])
           |	 strict a3 = invoke(dApp2,  "call", [], [])
           |	 strict b3 = invoke(dApp3,  "call", [], [])
           |	 strict a4 = invoke(dApp2,  "call", [], [])
           |	 strict b4 = invoke(dApp3,  "call", [], [])
           |	 strict a5 = invoke(dApp2,  "call", [], [])
           |	 strict b5 = invoke(dApp3,  "call", [], [])
           |	 strict a6 = invoke(dApp2,  "call", [], [])
           |	 strict b6 = invoke(dApp3,  "call", [], [])
           |	 strict a7 = invoke(dApp2,  "call", [], [])
           |	 strict b7 = invoke(dApp3,  "call", [], [])
           |	 strict a8 = invoke(dApp2,  "call", [], [])
           |	 strict b8 = invoke(dApp3,  "call", [], [])
           |	 strict a9 = invoke(dApp2,  "call", [], [])
           |	 strict b9 = invoke(dApp3,  "call", [], [])
           |	 strict a10 = invoke(dApp2,  "call", [], [])
           |	 strict b10 = invoke(dApp3,  "call", [], [])
           |	 strict a11 = invoke(dApp2,  "call", [], [])
           |	 strict b11 = invoke(dApp3,  "call", [], [])
           |	 []
           | }
         """.stripMargin
      )

    val dApp2Script =
      TestCompiler(V6).compileContract(
        s"""
           | {-# STDLIB_VERSION 6 #-}
           | {-# CONTENT_TYPE DAPP #-}
           | {-# SCRIPT_TYPE ACCOUNT #-}
           |
           | @Callable(i)
           | func call() = [
           |   ScriptTransfer(i.caller, 1, unit),
           |   ScriptTransfer(i.caller, 2, unit),
           |   ScriptTransfer(i.caller, 3, unit),
           |   ScriptTransfer(i.caller, 4, unit),
           |   ScriptTransfer(i.caller, 5, unit),
           |   ScriptTransfer(i.caller, 6, unit),
           |   ScriptTransfer(i.caller, 7, unit),
           |   ScriptTransfer(i.caller, 8, unit),
           |   ScriptTransfer(i.caller, 9, unit),
           |   ScriptTransfer(i.caller, 10, unit)
           | ]
         """.stripMargin
      )

    val dApp3Script =
      TestCompiler(V5).compileContract(
        s"""
           | {-# STDLIB_VERSION 5 #-}
           | {-# CONTENT_TYPE DAPP #-}
           | {-# SCRIPT_TYPE ACCOUNT #-}
           |
           | @Callable(i)
           | func call() = {
           |   strict a = i.caller
           |   []
           | }
         """.stripMargin
      )

    val setScripts = Seq(
      TxHelpers.setScript(dApp1, dApp1Script),
      TxHelpers.setScript(dApp2, dApp2Script),
      TxHelpers.setScript(dApp3, dApp3Script)
    )

    val invoke1 =
      TxHelpers.invoke(
        dApp1.toAddress,
        invoker = invoker,
        func = Some("call"),
        args = Seq()
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

    val featsWithRideV6   = feats ++ Seq(RideV6 -> 0)
    val setsEnabledRideV6 = TestFunctionalitySettings.withFeaturesByHeight(featsWithRideV6*)

    withDomain(domainSettingsWithFS(setsEnabledRideV6), balances) { d =>
      d.appendBlock(setScripts*)
      d.appendBlockE(invoke1) should beRight
    }
  }
}

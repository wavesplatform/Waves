package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, FUNCTION_CALL}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.{ENOUGH_AMT, produce}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.smart._
import com.wavesplatform.{NoShrink, TestTime, TransactionGen}
import org.scalatest.{EitherValues, Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SyncDAppRecursionTest
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with TransactionGen
    with NoShrink
    with WithDomain
    with EitherValues {

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private val features = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id    -> 0,
      BlockchainFeatures.SmartAssets.id      -> 0,
      BlockchainFeatures.Ride4DApps.id       -> 0,
      BlockchainFeatures.FeeSponsorship.id   -> 0,
      BlockchainFeatures.DataTransaction.id  -> 0,
      BlockchainFeatures.BlockReward.id      -> 0,
      BlockchainFeatures.BlockV5.id          -> 0,
      BlockchainFeatures.SynchronousCalls.id -> 0
    )
  )

  def dApp(nextDApp: Address): Script = TestCompiler(V5).compileContract(s"""
    | {-# STDLIB_VERSION 5       #-}
    | {-# CONTENT_TYPE   DAPP    #-}
    | {-# SCRIPT_TYPE    ACCOUNT #-}
    |
    | @Callable(i)
    | func default(end: Boolean) =
    |    if (end)
    |      then
    |        []
    |      else {
    |        let address = Address(base58'$nextDApp')
    |        strict r = Invoke(address, "default", [i.caller == this && address == this], [])
    |        []
    |      }
    |""".stripMargin)

  // A -> A -> B -> B
  property("dApp calls itself that calls other dApp that calls itself") {
    val preconditions =
      for {
        dApp1 <- accountGen
        dApp2 <- accountGen
        fee   <- ciFee()
        genesis1 = GenesisTransaction.create(dApp1.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis2 = GenesisTransaction.create(dApp2.toAddress, ENOUGH_AMT, ts).explicitGet()
        setDApp1 = SetScriptTransaction.selfSigned(1.toByte, dApp1, Some(dApp(dApp2.toAddress)), fee, ts).explicitGet()
        setDApp2 = SetScriptTransaction.selfSigned(1.toByte, dApp2, Some(dApp(dApp2.toAddress)), fee, ts).explicitGet()
        invoke = InvokeScriptTransaction
          .selfSigned(1.toByte, dApp1, dApp1.toAddress, Some(FUNCTION_CALL(User("default"), List(CONST_BOOLEAN(false)))), Nil, fee, Waves, ts)
          .explicitGet()
      } yield (List(genesis1, genesis2, setDApp1, setDApp2), invoke)

    val (preparingTxs, invoke) = preconditions.sample.get
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      features
    ) {
      case (diff, _) =>
        diff.errorMessage(invoke.id.value()) shouldBe None
        diff.scriptsRun shouldBe 4
    }
  }

  // A -> B -> C -> A
  property("dApp calls dApp chain with itself at the end") {
    val preconditions =
      for {
        dApp1 <- accountGen
        dApp2 <- accountGen
        dApp3 <- accountGen
        fee   <- ciFee()
        genesis1 = GenesisTransaction.create(dApp1.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis2 = GenesisTransaction.create(dApp2.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis3 = GenesisTransaction.create(dApp3.toAddress, ENOUGH_AMT, ts).explicitGet()
        setDApp1 = SetScriptTransaction.selfSigned(1.toByte, dApp1, Some(dApp(dApp1.toAddress)), fee, ts).explicitGet()
        setDApp2 = SetScriptTransaction.selfSigned(1.toByte, dApp2, Some(dApp(dApp3.toAddress)), fee, ts).explicitGet()
        setDApp3 = SetScriptTransaction.selfSigned(1.toByte, dApp3, Some(dApp(dApp1.toAddress)), fee, ts).explicitGet()
        invoke = InvokeScriptTransaction
          .selfSigned(1.toByte, dApp1, dApp2.toAddress, Some(FUNCTION_CALL(User("default"), List(CONST_BOOLEAN(false)))), Nil, fee, Waves, ts)
          .explicitGet()
      } yield (List(genesis1, genesis2, genesis3, setDApp1, setDApp2, setDApp3), invoke)

    val (preparingTxs, invoke) = preconditions.sample.get
    assertDiffEi(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      features
    )(
      _ should produce(
        s"The invocation stack contains multiple invocations of the dApp at address ${invoke.senderAddress} with invocations of another dApp between them"
      )
    )
  }

  // A -> B -> C -> B
  property("calling dApp called at the end") {
    val preconditions =
      for {
        dApp1 <- accountGen
        dApp2 <- accountGen
        dApp3 <- accountGen
        fee   <- ciFee()
        genesis1 = GenesisTransaction.create(dApp1.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis2 = GenesisTransaction.create(dApp2.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis3 = GenesisTransaction.create(dApp3.toAddress, ENOUGH_AMT, ts).explicitGet()
        setDApp1 = SetScriptTransaction.selfSigned(1.toByte, dApp1, Some(dApp(dApp1.toAddress)), fee, ts).explicitGet()
        setDApp2 = SetScriptTransaction.selfSigned(1.toByte, dApp2, Some(dApp(dApp3.toAddress)), fee, ts).explicitGet()
        setDApp3 = SetScriptTransaction.selfSigned(1.toByte, dApp3, Some(dApp(dApp2.toAddress)), fee, ts).explicitGet()
        invoke = InvokeScriptTransaction
          .selfSigned(1.toByte, dApp1, dApp2.toAddress, Some(FUNCTION_CALL(User("default"), List(CONST_BOOLEAN(false)))), Nil, fee, Waves, ts)
          .explicitGet()
      } yield (List(genesis1, genesis2, genesis3, setDApp1, setDApp2, setDApp3), invoke)

    val (preparingTxs, invoke) = preconditions.sample.get
    assertDiffEi(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      features
    )(
      _ should produce(
        s"The invocation stack contains multiple invocations of the dApp at address ${invoke.dAppAddressOrAlias} with invocations of another dApp between them"
      )
    )
  }

  // A -> B -> C -> D -> C
  property("dApp from chain called at the end") {
    val preconditions =
      for {
        dApp1 <- accountGen
        dApp2 <- accountGen
        dApp3 <- accountGen
        dApp4 <- accountGen
        fee   <- ciFee()
        genesis1 = GenesisTransaction.create(dApp1.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis2 = GenesisTransaction.create(dApp2.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis3 = GenesisTransaction.create(dApp3.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis4 = GenesisTransaction.create(dApp4.toAddress, ENOUGH_AMT, ts).explicitGet()
        setDApp1 = SetScriptTransaction.selfSigned(1.toByte, dApp1, Some(dApp(dApp1.toAddress)), fee, ts).explicitGet()
        setDApp2 = SetScriptTransaction.selfSigned(1.toByte, dApp2, Some(dApp(dApp3.toAddress)), fee, ts).explicitGet()
        setDApp3 = SetScriptTransaction.selfSigned(1.toByte, dApp3, Some(dApp(dApp4.toAddress)), fee, ts).explicitGet()
        setDApp4 = SetScriptTransaction.selfSigned(1.toByte, dApp4, Some(dApp(dApp3.toAddress)), fee, ts).explicitGet()
        invoke = InvokeScriptTransaction
          .selfSigned(1.toByte, dApp1, dApp2.toAddress, Some(FUNCTION_CALL(User("default"), List(CONST_BOOLEAN(false)))), Nil, fee, Waves, ts)
          .explicitGet()
      } yield (List(genesis1, genesis2, genesis3, genesis4, setDApp1, setDApp2, setDApp3, setDApp4), invoke)

    val (preparingTxs @ _ :+ (setDApp3: SetScriptTransaction) :+ _, invoke) = preconditions.sample.get
    assertDiffEi(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      features
    )(
      _ should produce(
        s"The invocation stack contains multiple invocations of the dApp at address ${setDApp3.sender.toAddress} with invocations of another dApp between them"
      )
    )
  }
}

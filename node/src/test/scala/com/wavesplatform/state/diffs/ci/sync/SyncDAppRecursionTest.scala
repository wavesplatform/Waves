package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, FUNCTION_CALL}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.smart._
import com.wavesplatform.transaction.utils.Signed

class SyncDAppRecursionTest extends PropSpec with WithDomain {
  import DomainPresets._

  private val features = RideV6.blockchainSettings.functionalitySettings

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private val fc = Some(FUNCTION_CALL(User("default"), (1 to 4).map(_ => CONST_BOOLEAN(false)).toList))

  def dApp(
      nextDApp: Address,
      sendEnd: Boolean = false,
      reentrant: Boolean = false,
      secondNextDApp: Option[Address] = None,
      sendEndToNext: Boolean = false,
      sendChangeDApp: Boolean = false,
      sendForceInvoke: Boolean = false,
      sendForceReentrant: Boolean = false,
      secondReentrantInvoke: Option[Address] = None,
      invokeExpression: Boolean = false
  ): Script = {
    val func    = if (reentrant) "reentrantInvoke" else "invoke"
    val args    = s"[sendEnd, $sendChangeDApp, $sendForceInvoke, $sendForceReentrant]"
    val compile = if (invokeExpression) TestCompiler(V5).compileFreeCall(_) else TestCompiler(V5).compileContract(_, false)
    val prefix =
      if (invokeExpression)
        "let (end, useSecondAddress, forceInvoke, forceReentrant) = (false, false, false, false)"
      else
        """
          | {-# STDLIB_VERSION 5       #-}
          | {-# CONTENT_TYPE   DAPP    #-}
          | {-# SCRIPT_TYPE    ACCOUNT #-}
          |
          | @Callable(i)
          | func default(end: Boolean, useSecondAddress: Boolean, forceInvoke: Boolean, forceReentrant: Boolean) =
        """.stripMargin
    compile(
      s"""
         | $prefix
         | if (end)
         |   then
         |     []
         |   else {
         |     let endWithNextDApp = useSecondAddress && $sendEndToNext
         |     let sendEnd = $sendEnd || endWithNextDApp
         |     let address = ${secondNextDApp.fold("")(a => s"if (useSecondAddress) then Address(base58'$a') else")} Address(base58'$nextDApp')
         |     strict r =
         |       if (forceInvoke || endWithNextDApp) # param 'endWithNextDApp' is used for self-recursion check without reentrancy
         |         then
         |           invoke(address, "default", $args, [])
         |         else if (forceReentrant)
         |           then
         |             reentrantInvoke(address, "default", $args, [])
         |           else
         |             $func(address, "default", $args, [])
         |
         |     ${secondReentrantInvoke.fold("")(a => s""" strict r2 = reentrantInvoke(Address(base58'$a'), "default", $args, []) """)}
         |     []
         |   }
       """.stripMargin
    )
  }

  // A -> A -> B -> B
  property("dApp calls itself that calls other dApp that calls itself - allowed") {
    def preconditions(invokeExpression: Boolean) =
      for {
        dApp1 <- accountGen
        dApp2 <- accountGen
        fee   <- ciFee(freeCall = invokeExpression)
        genesis1 = GenesisTransaction.create(dApp1.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis2 = GenesisTransaction.create(dApp2.toAddress, ENOUGH_AMT, ts).explicitGet()
        setDApp1 = SetScriptTransaction.selfSigned(1.toByte, dApp1, Some(dApp(dApp2.toAddress)), fee, ts).explicitGet()
        setDApp2 = SetScriptTransaction.selfSigned(1.toByte, dApp2, Some(dApp(dApp2.toAddress, sendEnd = true)), fee, ts).explicitGet()
        invoke = if (invokeExpression)
          InvokeExpressionTransaction
            .selfSigned(1.toByte, dApp1, dApp(dApp2.toAddress, invokeExpression = invokeExpression).asInstanceOf[ExprScript], fee, Waves, ts)
            .explicitGet()
        else
          InvokeScriptTransaction
            .selfSigned(1.toByte, dApp1, dApp1.toAddress, fc, Nil, fee, Waves, ts)
            .explicitGet()
      } yield (List(genesis1, genesis2, setDApp1, setDApp2), invoke)

    Seq(true, false).foreach { invokeExpression =>
      val (preparingTxs, invoke) = preconditions(invokeExpression).sample.get
      assertDiffAndState(
        Seq(TestBlock.create(preparingTxs)),
        TestBlock.create(Seq(invoke)),
        features
      ) {
        case (diff, _) =>
          diff.errorMessage(invoke.id()) shouldBe None
          diff.scriptsRun shouldBe 3
      }
    }
  }

  // A -> B -> C -> A
  property("dApp calls dApp chain with itself at the end - prohibited") {
    def preconditions(invokeExpression: Boolean) =
      for {
        dApp1 <- accountGen
        dApp2 <- accountGen
        dApp3 <- accountGen
        fee   <- ciFee(freeCall = invokeExpression)
        genesis1 = GenesisTransaction.create(dApp1.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis2 = GenesisTransaction.create(dApp2.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis3 = GenesisTransaction.create(dApp3.toAddress, ENOUGH_AMT, ts).explicitGet()
        setDApp1 = SetScriptTransaction.selfSigned(1.toByte, dApp1, Some(dApp(dApp1.toAddress)), fee, ts).explicitGet()
        setDApp2 = SetScriptTransaction.selfSigned(1.toByte, dApp2, Some(dApp(dApp3.toAddress)), fee, ts).explicitGet()
        setDApp3 = SetScriptTransaction.selfSigned(1.toByte, dApp3, Some(dApp(dApp1.toAddress)), fee, ts).explicitGet()
        invoke = if (invokeExpression)
          InvokeExpressionTransaction
            .selfSigned(1.toByte, dApp1, dApp(dApp2.toAddress, invokeExpression = invokeExpression).asInstanceOf[ExprScript], fee, Waves, ts)
            .explicitGet()
        else
          InvokeScriptTransaction
            .selfSigned(1.toByte, dApp1, dApp2.toAddress, fc, Nil, fee, Waves, ts)
            .explicitGet()
      } yield (List(genesis1, genesis2, genesis3, setDApp1, setDApp2, setDApp3), invoke)

    Seq(true, false).foreach { invokeExpression =>
      val (preparingTxs, invoke) = preconditions(invokeExpression).sample.get
      assertDiffEi(
        Seq(TestBlock.create(preparingTxs)),
        TestBlock.create(Seq(invoke)),
        features
      )(
        _ should produce(
          s"The invocation stack contains multiple invocations " +
            s"of the dApp at address ${invoke.senderAddress} with " +
            s"invocations of another dApp between them"
        )
      )
    }
  }

  // 2 scenarios:
  // A -> B -> C -> B
  // A -> B -> C -[r]-> B
  property("calling dApp is called again - prohibited") {
    def assert(reentrant: Boolean): Unit = {
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
          setDApp3 = SetScriptTransaction.selfSigned(1.toByte, dApp3, Some(dApp(dApp2.toAddress, reentrant = reentrant)), fee, ts).explicitGet()
          invoke   = Signed.invokeScript(1.toByte, dApp1, dApp2.toAddress, fc, Nil, fee, Waves, ts)
        } yield (List(genesis1, genesis2, genesis3, setDApp1, setDApp2, setDApp3), invoke)

      val (preparingTxs, invoke) = preconditions.sample.get
      assertDiffEi(
        Seq(TestBlock.create(preparingTxs)),
        TestBlock.create(Seq(invoke)),
        features
      )(
        _ should produce(
          s"The invocation stack contains multiple invocations of the dApp at address ${invoke.dApp} with invocations of another dApp between them"
        )
      )
    }
    assert(reentrant = false)
    assert(reentrant = true)
  }

  // A -> B -> B -[r]-> C -[r]-> B
  property("calling twice dApp is called again after reentrant calls - prohibited") {
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
        setDApp2 = SetScriptTransaction
          .selfSigned(
            1.toByte,
            dApp2,
            Some(dApp(dApp2.toAddress, secondNextDApp = Some(dApp3.toAddress), sendChangeDApp = true, sendForceReentrant = true)),
            fee,
            ts
          )
          .explicitGet()
        setDApp3 = SetScriptTransaction
          .selfSigned(1.toByte, dApp3, Some(dApp(dApp2.toAddress, reentrant = true, sendEnd = true)), fee, ts)
          .explicitGet()
        invoke = Signed.invokeScript(1.toByte, dApp1, dApp2.toAddress, fc, Nil, fee, Waves, ts)
      } yield (List(genesis1, genesis2, genesis3, setDApp1, setDApp2, setDApp3), invoke)

    val (preparingTxs, invoke) = preconditions.sample.get
    assertDiffEi(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      features
    )(
      _ should produce(
        s"The invocation stack contains multiple invocations of the dApp at address ${invoke.dApp} with invocations of another dApp between them"
      )
    )
  }

  // A -> B -> C -> D -> C
  property("dApp from chain is called again - prohibited") {
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
        invoke   = Signed.invokeScript(1.toByte, dApp1, dApp2.toAddress, fc, Nil, fee, Waves, ts)
      } yield (List(genesis1, genesis2, genesis3, genesis4, setDApp1, setDApp2, setDApp3, setDApp4), setDApp3, invoke)

    val (preparingTxs, setDApp3, invoke) = preconditions.sample.get
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

  // A -> B -> C -[r]-> D -> C
  property("dApp is called after reentrant call - allowed") {
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
        setDApp3 = SetScriptTransaction.selfSigned(1.toByte, dApp3, Some(dApp(dApp4.toAddress, reentrant = true)), fee, ts).explicitGet()
        setDApp4 = SetScriptTransaction.selfSigned(1.toByte, dApp4, Some(dApp(dApp3.toAddress, sendEnd = true)), fee, ts).explicitGet()
        invoke   = Signed.invokeScript(1.toByte, dApp1, dApp2.toAddress, fc, Nil, fee, Waves, ts)
      } yield (List(genesis1, genesis2, genesis3, genesis4, setDApp1, setDApp2, setDApp3, setDApp4), invoke)

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

  // A -> B -[r]-> C -> D -> B
  property("dApp is called after reentrant and usual call other dApp - allowed") {
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
        setDApp2 = SetScriptTransaction.selfSigned(1.toByte, dApp2, Some(dApp(dApp3.toAddress, reentrant = true)), fee, ts).explicitGet()
        setDApp3 = SetScriptTransaction.selfSigned(1.toByte, dApp3, Some(dApp(dApp4.toAddress)), fee, ts).explicitGet()
        setDApp4 = SetScriptTransaction.selfSigned(1.toByte, dApp4, Some(dApp(dApp2.toAddress, sendEnd = true)), fee, ts).explicitGet()
        invoke   = Signed.invokeScript(1.toByte, dApp1, dApp2.toAddress, fc, Nil, fee, Waves, ts)
      } yield (List(genesis1, genesis2, genesis3, genesis4, setDApp1, setDApp2, setDApp3, setDApp4), invoke)

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

  // A -> B -[r]-> C -> B -> B
  property("dApp is called from itself after reentrant call - allowed") {
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
        setDApp2 = SetScriptTransaction
          .selfSigned(
            1.toByte,
            dApp2,
            Some(dApp(dApp3.toAddress, reentrant = true, secondNextDApp = Some(dApp2.toAddress), sendEndToNext = true)),
            fee,
            ts
          )
          .explicitGet()
        setDApp3 = SetScriptTransaction.selfSigned(1.toByte, dApp3, Some(dApp(dApp2.toAddress, sendChangeDApp = true)), fee, ts).explicitGet()
        invoke   = Signed.invokeScript(1.toByte, dApp1, dApp2.toAddress, fc, Nil, fee, Waves, ts)
      } yield (List(genesis1, genesis2, genesis3, setDApp1, setDApp2, setDApp3), invoke)

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

  // A -> B -[r]-> C -> D -> C
  property("dApp that called from reentrant dApp is called again - prohibited") {
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
        setDApp2 = SetScriptTransaction.selfSigned(1.toByte, dApp2, Some(dApp(dApp3.toAddress, reentrant = true)), fee, ts).explicitGet()
        setDApp3 = SetScriptTransaction.selfSigned(1.toByte, dApp3, Some(dApp(dApp4.toAddress)), fee, ts).explicitGet()
        setDApp4 = SetScriptTransaction.selfSigned(1.toByte, dApp4, Some(dApp(dApp3.toAddress, sendEnd = true)), fee, ts).explicitGet()
        invoke   = Signed.invokeScript(1.toByte, dApp1, dApp2.toAddress, fc, Nil, fee, Waves, ts)
      } yield (List(genesis1, genesis2, genesis3, genesis4, setDApp1, setDApp2, setDApp3, setDApp4), invoke, dApp3.toAddress)

    forAll(preconditions) {
      case (preparingTxs, invoke, addr) =>
        assertDiffEi(
          Seq(TestBlock.create(preparingTxs)),
          TestBlock.create(Seq(invoke)),
          features
        )(
          _ should produce(
            s"The invocation stack contains multiple invocations of the dApp at address $addr with invocations of another dApp between them"
          )
        )
    }
  }

  // A -> B -[r]-> C -> D -> B -[r]-> E -> B
  property("dApp is called after 2 reentrant call - allowed") {
    val preconditions =
      for {
        dApp1 <- accountGen
        dApp2 <- accountGen
        dApp3 <- accountGen
        dApp4 <- accountGen
        dApp5 <- accountGen
        fee   <- ciFee()
        genesis1 = GenesisTransaction.create(dApp1.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis2 = GenesisTransaction.create(dApp2.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis3 = GenesisTransaction.create(dApp3.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis4 = GenesisTransaction.create(dApp4.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis5 = GenesisTransaction.create(dApp5.toAddress, ENOUGH_AMT, ts).explicitGet()
        setDApp1 = SetScriptTransaction.selfSigned(1.toByte, dApp1, Some(dApp(dApp1.toAddress)), fee, ts).explicitGet()
        setDApp2 = SetScriptTransaction
          .selfSigned(1.toByte, dApp2, Some(dApp(dApp3.toAddress, reentrant = true, secondNextDApp = Some(dApp5.toAddress))), fee, ts)
          .explicitGet()
        setDApp3 = SetScriptTransaction.selfSigned(1.toByte, dApp3, Some(dApp(dApp4.toAddress)), fee, ts).explicitGet()
        setDApp4 = SetScriptTransaction.selfSigned(1.toByte, dApp4, Some(dApp(dApp2.toAddress, sendChangeDApp = true)), fee, ts).explicitGet()
        setDApp5 = SetScriptTransaction.selfSigned(1.toByte, dApp5, Some(dApp(dApp2.toAddress, sendEnd = true)), fee, ts).explicitGet()
        invoke   = Signed.invokeScript(1.toByte, dApp1, dApp2.toAddress, fc, Nil, fee, Waves, ts)
      } yield (List(genesis1, genesis2, genesis3, genesis4, genesis5, setDApp1, setDApp2, setDApp3, setDApp4, setDApp5), invoke)

    val (preparingTxs, invoke) = preconditions.sample.get
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      features
    ) {
      case (diff, _) =>
        diff.errorMessage(invoke.id.value()) shouldBe None
        diff.scriptsRun shouldBe 6
    }
  }

  // A -> B -[r]-> C -> D -> B -> E -> B
  property("dApp is called after reentrant and simple call - prohibited") {
    val preconditions =
      for {
        dApp1 <- accountGen
        dApp2 <- accountGen
        dApp3 <- accountGen
        dApp4 <- accountGen
        dApp5 <- accountGen
        fee   <- ciFee()
        genesis1 = GenesisTransaction.create(dApp1.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis2 = GenesisTransaction.create(dApp2.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis3 = GenesisTransaction.create(dApp3.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis4 = GenesisTransaction.create(dApp4.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis5 = GenesisTransaction.create(dApp5.toAddress, ENOUGH_AMT, ts).explicitGet()
        setDApp1 = SetScriptTransaction.selfSigned(1.toByte, dApp1, Some(dApp(dApp1.toAddress)), fee, ts).explicitGet()
        setDApp2 = SetScriptTransaction
          .selfSigned(1.toByte, dApp2, Some(dApp(dApp3.toAddress, reentrant = true, secondNextDApp = Some(dApp5.toAddress))), fee, ts)
          .explicitGet()
        setDApp3 = SetScriptTransaction.selfSigned(1.toByte, dApp3, Some(dApp(dApp4.toAddress)), fee, ts).explicitGet()
        setDApp4 = SetScriptTransaction
          .selfSigned(1.toByte, dApp4, Some(dApp(dApp2.toAddress, sendChangeDApp = true, sendForceInvoke = true)), fee, ts)
          .explicitGet()
        setDApp5 = SetScriptTransaction.selfSigned(1.toByte, dApp5, Some(dApp(dApp2.toAddress, sendEnd = true)), fee, ts).explicitGet()
        invoke   = Signed.invokeScript(1.toByte, dApp1, dApp2.toAddress, fc, Nil, fee, Waves, ts)
      } yield (List(genesis1, genesis2, genesis3, genesis4, genesis5, setDApp1, setDApp2, setDApp3, setDApp4, setDApp5), invoke, dApp2.toAddress)

    val (preparingTxs, invoke, errorAddress) = preconditions.sample.get
    assertDiffEi(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      features
    )(
      _ should produce(
        s"The invocation stack contains multiple invocations of the dApp at address $errorAddress with invocations of another dApp between them"
      )
    )
  }

  // A -> B -> C -> D
  //        -[r]-> E -> C -> B
  property("dApps (B - reentrant, C - simple) are called after calls from new chain - allowed") {
    val preconditions =
      for {
        dApp1 <- accountGen
        dApp2 <- accountGen
        dApp3 <- accountGen
        dApp4 <- accountGen
        dApp5 <- accountGen
        fee   <- ciFee()
        genesis1 = GenesisTransaction.create(dApp1.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis2 = GenesisTransaction.create(dApp2.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis3 = GenesisTransaction.create(dApp3.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis4 = GenesisTransaction.create(dApp4.toAddress, ENOUGH_AMT, ts).explicitGet()
        genesis5 = GenesisTransaction.create(dApp5.toAddress, ENOUGH_AMT, ts).explicitGet()
        setDApp1 = SetScriptTransaction.selfSigned(1.toByte, dApp1, Some(dApp(dApp1.toAddress)), fee, ts).explicitGet()
        setDApp2 = SetScriptTransaction
          .selfSigned(1.toByte, dApp2, Some(dApp(dApp3.toAddress, secondReentrantInvoke = Some(dApp5.toAddress))), fee, ts)
          .explicitGet()
        setDApp3 = SetScriptTransaction
          .selfSigned(1.toByte, dApp3, Some(dApp(dApp4.toAddress, sendEnd = true, secondNextDApp = Some(dApp2.toAddress))), fee, ts)
          .explicitGet()
        setDApp4 = SetScriptTransaction.selfSigned(1.toByte, dApp4, Some(dApp(dApp2.toAddress)), fee, ts).explicitGet()
        setDApp5 = SetScriptTransaction.selfSigned(1.toByte, dApp5, Some(dApp(dApp3.toAddress, sendChangeDApp = true)), fee, ts).explicitGet()
        invoke   = Signed.invokeScript(1.toByte, dApp1, dApp2.toAddress, fc, Nil, fee, Waves, ts)
      } yield (List(genesis1, genesis2, genesis3, genesis4, genesis5, setDApp1, setDApp2, setDApp3, setDApp4, setDApp5), invoke)

    val (preparingTxs, invoke) = preconditions.sample.get
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      features
    ) {
      case (diff, _) =>
        diff.errorMessage(invoke.id.value()) shouldBe None
        diff.scriptsRun shouldBe 6
    }
  }
}

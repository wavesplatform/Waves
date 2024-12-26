package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.account.Address
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, FUNCTION_CALL}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.parser.Parser.LibrariesOffset.NoLibraries
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.test.*
import com.wavesplatform.transaction.{TxHelpers, TxVersion}
import org.scalatest.Inside

class SyncDAppRecursionTest extends PropSpec with WithDomain with Inside {
  import DomainPresets.*

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
    val func = if (reentrant) "reentrantInvoke" else "invoke"
    val args = s"[sendEnd, $sendChangeDApp, $sendForceInvoke, $sendForceReentrant]"
    val compile =
      if (invokeExpression) TestCompiler(V5).compileFreeCall(_, offset = NoLibraries)
      else TestCompiler(V5).compileContract(_, offset = NoLibraries, allowIllFormedStrings = false, compact = false)
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
    def preconditions(invokeExpression: Boolean) = {
      val dApp1 = TxHelpers.signer(1)
      val dApp2 = TxHelpers.signer(2)

      val fee = TxHelpers.ciFee(freeCall = invokeExpression)

      val genesis  = Seq(dApp1, dApp2).map(acc => TxHelpers.genesis(acc.toAddress))
      val setDApp1 = TxHelpers.setScript(dApp1, dApp(dApp2.toAddress))
      val setDApp2 = TxHelpers.setScript(dApp2, dApp(dApp2.toAddress, sendEnd = true))
      val invoke = if (invokeExpression) {
        TxHelpers.invokeExpression(dApp(dApp2.toAddress, invokeExpression = invokeExpression).asInstanceOf[ExprScript], dApp1, fee = fee)
      } else {
        TxHelpers.invoke(
          dApp1.toAddress,
          func = fc.map(_.function.funcName),
          args = fc.map(_.args).toList.flatten,
          invoker = dApp1,
          fee = fee,
          version = TxVersion.V1
        )
      }

      (genesis :+ setDApp1 :+ setDApp2, invoke)
    }

    Seq(true, false).foreach { invokeExpression =>
      val (preparingTxs, invoke) = preconditions(invokeExpression)
      assertDiffAndState(
        Seq(TestBlock.create(preparingTxs)),
        TestBlock.create(Seq(invoke)),
        features(invokeExpression)
      ) { case (snapshot, _) =>
        snapshot.errorMessage(invoke.id()) shouldBe None
        inside(snapshot.scriptResults.toSeq) { case Seq((_, call1)) =>
          inside(call1.invokes) { case Seq(call2) =>
            inside(call2.stateChanges.invokes) { case Seq(call3) =>
              call3.stateChanges.error shouldBe empty
              call3.stateChanges.invokes shouldBe empty
            }
          }
        }
      }
    }
  }

  // A -> B -> C -> A
  property("dApp calls dApp chain with itself at the end - prohibited") {
    def preconditions(invokeExpression: Boolean) = {
      val dApp1 = TxHelpers.signer(1)
      val dApp2 = TxHelpers.signer(2)
      val dApp3 = TxHelpers.signer(3)

      val fee = TxHelpers.ciFee(freeCall = invokeExpression)

      val genesis  = Seq(dApp1, dApp2, dApp3).map(acc => TxHelpers.genesis(acc.toAddress))
      val setDApp1 = TxHelpers.setScript(dApp1, dApp(dApp1.toAddress))
      val setDApp2 = TxHelpers.setScript(dApp2, dApp(dApp3.toAddress))
      val setDApp3 = TxHelpers.setScript(dApp3, dApp(dApp1.toAddress))
      val invoke = if (invokeExpression) {
        TxHelpers.invokeExpression(dApp(dApp2.toAddress, invokeExpression = invokeExpression).asInstanceOf[ExprScript], dApp1, fee = fee)
      } else {
        TxHelpers.invoke(
          dApp2.toAddress,
          func = fc.map(_.function.funcName),
          args = fc.map(_.args).toList.flatten,
          invoker = dApp1,
          fee = fee,
          version = TxVersion.V1
        )
      }

      (genesis :+ setDApp1 :+ setDApp2 :+ setDApp3, invoke)
    }

    Seq(true, false).foreach { invokeExpression =>
      val (preparingTxs, invoke) = preconditions(invokeExpression)
      assertDiffEi(
        Seq(TestBlock.create(preparingTxs)),
        TestBlock.create(Seq(invoke)),
        features(invokeExpression)
      )(
        _ should produce(
          s"The invocation stack contains multiple invocations " +
            s"of the dApp at address ${invoke.sender.toAddress} with " +
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
      val preconditions = {
        val dApp1 = TxHelpers.signer(1)
        val dApp2 = TxHelpers.signer(2)
        val dApp3 = TxHelpers.signer(3)

        val genesis  = Seq(dApp1, dApp2, dApp3).map(acc => TxHelpers.genesis(acc.toAddress))
        val setDApp1 = TxHelpers.setScript(dApp1, dApp(dApp1.toAddress))
        val setDApp2 = TxHelpers.setScript(dApp2, dApp(dApp3.toAddress))
        val setDApp3 = TxHelpers.setScript(dApp3, dApp(dApp2.toAddress))
        val invoke = TxHelpers.invoke(
          dApp2.toAddress,
          func = fc.map(_.function.funcName),
          args = fc.map(_.args).toList.flatten,
          invoker = dApp1,
          version = TxVersion.V1
        )

        (genesis :+ setDApp1 :+ setDApp2 :+ setDApp3, invoke)
      }

      val (preparingTxs, invoke) = preconditions
      assertDiffEi(
        Seq(TestBlock.create(preparingTxs)),
        TestBlock.create(Seq(invoke)),
        features()
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
    val preconditions = {
      val dApp1 = TxHelpers.signer(1)
      val dApp2 = TxHelpers.signer(2)
      val dApp3 = TxHelpers.signer(3)

      val genesis  = Seq(dApp1, dApp2, dApp3).map(acc => TxHelpers.genesis(acc.toAddress))
      val setDApp1 = TxHelpers.setScript(dApp1, dApp(dApp1.toAddress))
      val setDApp2 =
        TxHelpers.setScript(dApp2, dApp(dApp2.toAddress, secondNextDApp = Some(dApp3.toAddress), sendChangeDApp = true, sendForceReentrant = true))
      val setDApp3 = TxHelpers.setScript(dApp3, dApp(dApp2.toAddress, reentrant = true, sendEnd = true))
      val invoke = TxHelpers.invoke(
        dApp2.toAddress,
        func = fc.map(_.function.funcName),
        args = fc.map(_.args).toList.flatten,
        invoker = dApp1,
        version = TxVersion.V1
      )

      (genesis :+ setDApp1 :+ setDApp2 :+ setDApp3, invoke)
    }

    val (preparingTxs, invoke) = preconditions
    assertDiffEi(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      features()
    )(
      _ should produce(
        s"The invocation stack contains multiple invocations of the dApp at address ${invoke.dApp} with invocations of another dApp between them"
      )
    )
  }

  // A -> B -> C -> D -> C
  property("dApp from chain is called again - prohibited") {
    val preconditions = {
      val dApp1 = TxHelpers.signer(1)
      val dApp2 = TxHelpers.signer(2)
      val dApp3 = TxHelpers.signer(3)
      val dApp4 = TxHelpers.signer(4)

      val genesis  = Seq(dApp1, dApp2, dApp3, dApp4).map(acc => TxHelpers.genesis(acc.toAddress))
      val setDApp1 = TxHelpers.setScript(dApp1, dApp(dApp1.toAddress))
      val setDApp2 = TxHelpers.setScript(dApp2, dApp(dApp3.toAddress))
      val setDApp3 = TxHelpers.setScript(dApp3, dApp(dApp4.toAddress))
      val setDApp4 = TxHelpers.setScript(dApp4, dApp(dApp3.toAddress))
      val invoke = TxHelpers.invoke(
        dApp2.toAddress,
        func = fc.map(_.function.funcName),
        args = fc.map(_.args).toList.flatten,
        invoker = dApp1,
        version = TxVersion.V1
      )

      (genesis :+ setDApp1 :+ setDApp2 :+ setDApp3 :+ setDApp4, setDApp3, invoke)
    }

    val (preparingTxs, setDApp3, invoke) = preconditions
    assertDiffEi(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      features()
    )(
      _ should produce(
        s"The invocation stack contains multiple invocations of the dApp at address ${setDApp3.sender.toAddress} with invocations of another dApp between them"
      )
    )
  }

  // A -> B -> C -[r]-> D -> C
  property("dApp is called after reentrant call - allowed") {
    val preconditions = {
      val dApp1 = TxHelpers.signer(1)
      val dApp2 = TxHelpers.signer(2)
      val dApp3 = TxHelpers.signer(3)
      val dApp4 = TxHelpers.signer(4)

      val genesis  = Seq(dApp1, dApp2, dApp3, dApp4).map(acc => TxHelpers.genesis(acc.toAddress))
      val setDApp1 = TxHelpers.setScript(dApp1, dApp(dApp1.toAddress))
      val setDApp2 = TxHelpers.setScript(dApp2, dApp(dApp3.toAddress))
      val setDApp3 = TxHelpers.setScript(dApp3, dApp(dApp4.toAddress, reentrant = true))
      val setDApp4 = TxHelpers.setScript(dApp4, dApp(dApp3.toAddress, sendEnd = true))
      val invoke = TxHelpers.invoke(
        dApp2.toAddress,
        func = fc.map(_.function.funcName),
        args = fc.map(_.args).toList.flatten,
        invoker = dApp1,
        version = TxVersion.V1
      )

      (genesis :+ setDApp1 :+ setDApp2 :+ setDApp3 :+ setDApp4, invoke)
    }

    val (preparingTxs, invoke) = preconditions
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      features()
    ) { case (snapshot, _) =>
      snapshot.errorMessage(invoke.id.value()) shouldBe None
      inside(snapshot.scriptResults.toSeq) { case Seq((_, call1)) =>
        inside(call1.invokes) { case Seq(call2) =>
          inside(call2.stateChanges.invokes) { case Seq(call3) =>
            inside(call3.stateChanges.invokes) { case Seq(call4) =>
              call4.stateChanges.error shouldBe empty
              call4.stateChanges.invokes shouldBe empty
            }
          }
        }
      }
    }
  }

  // A -> B -[r]-> C -> D -> B
  property("dApp is called after reentrant and usual call other dApp - allowed") {
    val preconditions = {
      val dApp1 = TxHelpers.signer(1)
      val dApp2 = TxHelpers.signer(2)
      val dApp3 = TxHelpers.signer(3)
      val dApp4 = TxHelpers.signer(4)

      val genesis  = Seq(dApp1, dApp2, dApp3, dApp4).map(acc => TxHelpers.genesis(acc.toAddress))
      val setDApp1 = TxHelpers.setScript(dApp1, dApp(dApp1.toAddress))
      val setDApp2 = TxHelpers.setScript(dApp2, dApp(dApp3.toAddress, reentrant = true))
      val setDApp3 = TxHelpers.setScript(dApp3, dApp(dApp4.toAddress))
      val setDApp4 = TxHelpers.setScript(dApp4, dApp(dApp2.toAddress, sendEnd = true))
      val invoke = TxHelpers.invoke(
        dApp2.toAddress,
        func = fc.map(_.function.funcName),
        args = fc.map(_.args).toList.flatten,
        invoker = dApp1,
        version = TxVersion.V1
      )

      (genesis :+ setDApp1 :+ setDApp2 :+ setDApp3 :+ setDApp4, invoke)
    }

    val (preparingTxs, invoke) = preconditions
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      features()
    ) { case (snapshot, _) =>
      snapshot.errorMessage(invoke.id.value()) shouldBe None
      inside(snapshot.scriptResults.toSeq) { case Seq((_, call1)) =>
        inside(call1.invokes) { case Seq(call2) =>
          inside(call2.stateChanges.invokes) { case Seq(call3) =>
            inside(call3.stateChanges.invokes) { case Seq(call4) =>
              call4.stateChanges.error shouldBe empty
              call4.stateChanges.invokes shouldBe empty
            }
          }
        }
      }
    }
  }

  // A -> B -[r]-> C -> B -> B
  property("dApp is called from itself after reentrant call - allowed") {
    val preconditions = {
      val dApp1 = TxHelpers.signer(1)
      val dApp2 = TxHelpers.signer(2)
      val dApp3 = TxHelpers.signer(3)

      val genesis  = Seq(dApp1, dApp2, dApp3).map(acc => TxHelpers.genesis(acc.toAddress))
      val setDApp1 = TxHelpers.setScript(dApp1, dApp(dApp1.toAddress))
      val setDApp2 = TxHelpers.setScript(dApp2, dApp(dApp3.toAddress, reentrant = true, secondNextDApp = Some(dApp2.toAddress), sendEndToNext = true))
      val setDApp3 = TxHelpers.setScript(dApp3, dApp(dApp2.toAddress, sendChangeDApp = true))
      val invoke = TxHelpers.invoke(
        dApp2.toAddress,
        func = fc.map(_.function.funcName),
        args = fc.map(_.args).toList.flatten,
        invoker = dApp1,
        version = TxVersion.V1
      )

      (genesis :+ setDApp1 :+ setDApp2 :+ setDApp3, invoke)
    }

    val (preparingTxs, invoke) = preconditions
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      features()
    ) { case (snapshot, _) =>
      snapshot.errorMessage(invoke.id.value()) shouldBe None
      inside(snapshot.scriptResults.toSeq) { case Seq((_, call1)) =>
        inside(call1.invokes) { case Seq(call2) =>
          inside(call2.stateChanges.invokes) { case Seq(call3) =>
            inside(call3.stateChanges.invokes) { case Seq(call4) =>
              call4.stateChanges.error shouldBe empty
              call4.stateChanges.invokes shouldBe empty
            }
          }
        }
      }
    }
  }

  // A -> B -[r]-> C -> D -> C
  property("dApp that called from reentrant dApp is called again - prohibited") {
    val preconditions = {
      val dApp1 = TxHelpers.signer(1)
      val dApp2 = TxHelpers.signer(2)
      val dApp3 = TxHelpers.signer(3)
      val dApp4 = TxHelpers.signer(4)

      val genesis  = Seq(dApp1, dApp2, dApp3, dApp4).map(acc => TxHelpers.genesis(acc.toAddress))
      val setDApp1 = TxHelpers.setScript(dApp1, dApp(dApp1.toAddress))
      val setDApp2 = TxHelpers.setScript(dApp2, dApp(dApp3.toAddress, reentrant = true))
      val setDApp3 = TxHelpers.setScript(dApp3, dApp(dApp4.toAddress))
      val setDApp4 = TxHelpers.setScript(dApp4, dApp(dApp3.toAddress, sendEnd = true))
      val invoke = TxHelpers.invoke(
        dApp2.toAddress,
        func = fc.map(_.function.funcName),
        args = fc.map(_.args).toList.flatten,
        invoker = dApp1,
        version = TxVersion.V1
      )

      (genesis :+ setDApp1 :+ setDApp2 :+ setDApp3 :+ setDApp4, invoke, dApp3.toAddress)
    }

    val (preparingTxs, invoke, addr) = preconditions
    assertDiffEi(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      features()
    )(
      _ should produce(
        s"The invocation stack contains multiple invocations of the dApp at address $addr with invocations of another dApp between them"
      )
    )
  }

  // A -> B -[r]-> C -> D -> B -[r]-> E -> B
  property("dApp is called after 2 reentrant call - allowed") {
    val preconditions = {
      val dApp1 = TxHelpers.signer(1)
      val dApp2 = TxHelpers.signer(2)
      val dApp3 = TxHelpers.signer(3)
      val dApp4 = TxHelpers.signer(4)
      val dApp5 = TxHelpers.signer(5)

      val genesis  = Seq(dApp1, dApp2, dApp3, dApp4, dApp5).map(acc => TxHelpers.genesis(acc.toAddress))
      val setDApp1 = TxHelpers.setScript(dApp1, dApp(dApp1.toAddress))
      val setDApp2 = TxHelpers.setScript(dApp2, dApp(dApp3.toAddress, reentrant = true, secondNextDApp = Some(dApp5.toAddress)))
      val setDApp3 = TxHelpers.setScript(dApp3, dApp(dApp4.toAddress))
      val setDApp4 = TxHelpers.setScript(dApp4, dApp(dApp2.toAddress, sendChangeDApp = true))
      val setDApp5 = TxHelpers.setScript(dApp5, dApp(dApp2.toAddress, sendEnd = true))
      val invoke = TxHelpers.invoke(
        dApp2.toAddress,
        func = fc.map(_.function.funcName),
        args = fc.map(_.args).toList.flatten,
        invoker = dApp1,
        version = TxVersion.V1
      )

      (genesis :+ setDApp1 :+ setDApp2 :+ setDApp3 :+ setDApp4 :+ setDApp5, invoke)
    }

    val (preparingTxs, invoke) = preconditions
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      features()
    ) { case (snapshot, _) =>
      snapshot.errorMessage(invoke.id.value()) shouldBe None
      inside(snapshot.scriptResults.toSeq) { case Seq((_, call1)) =>
        inside(call1.invokes) { case Seq(call2) =>
          inside(call2.stateChanges.invokes) { case Seq(call3) =>
            inside(call3.stateChanges.invokes) { case Seq(call4) =>
              inside(call4.stateChanges.invokes) { case Seq(sync5) =>
                inside(sync5.stateChanges.invokes) { case Seq(sync6) =>
                  sync6.stateChanges.error shouldBe empty
                  sync6.stateChanges.invokes shouldBe empty
                }
              }
            }
          }
        }
      }
    }
  }

  // A -> B -[r]-> C -> D -> B -> E -> B
  property("dApp is called after reentrant and simple call - prohibited") {
    val preconditions = {
      val dApp1 = TxHelpers.signer(1)
      val dApp2 = TxHelpers.signer(2)
      val dApp3 = TxHelpers.signer(3)
      val dApp4 = TxHelpers.signer(4)
      val dApp5 = TxHelpers.signer(5)

      val genesis  = Seq(dApp1, dApp2, dApp3, dApp4, dApp5).map(acc => TxHelpers.genesis(acc.toAddress))
      val setDApp1 = TxHelpers.setScript(dApp1, dApp(dApp1.toAddress))
      val setDApp2 = TxHelpers.setScript(dApp2, dApp(dApp3.toAddress, reentrant = true, secondNextDApp = Some(dApp5.toAddress)))
      val setDApp3 = TxHelpers.setScript(dApp3, dApp(dApp4.toAddress))
      val setDApp4 = TxHelpers.setScript(dApp4, dApp(dApp2.toAddress, sendChangeDApp = true, sendForceInvoke = true))
      val setDApp5 = TxHelpers.setScript(dApp5, dApp(dApp2.toAddress, sendEnd = true))
      val invoke = TxHelpers.invoke(
        dApp2.toAddress,
        func = fc.map(_.function.funcName),
        args = fc.map(_.args).toList.flatten,
        invoker = dApp1,
        version = TxVersion.V1
      )

      (genesis :+ setDApp1 :+ setDApp2 :+ setDApp3 :+ setDApp4 :+ setDApp5, invoke, dApp2.toAddress)
    }

    val (preparingTxs, invoke, errorAddress) = preconditions
    assertDiffEi(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      features()
    )(
      _ should produce(
        s"The invocation stack contains multiple invocations of the dApp at address $errorAddress with invocations of another dApp between them"
      )
    )
  }

  // A -> B -> C -> D
  //        -[r]-> E -> C -> B
  property("dApps (B - reentrant, C - simple) are called after calls from new chain - allowed") {
    val preconditions = {
      val dApp1 = TxHelpers.signer(1)
      val dApp2 = TxHelpers.signer(2)
      val dApp3 = TxHelpers.signer(3)
      val dApp4 = TxHelpers.signer(4)
      val dApp5 = TxHelpers.signer(5)

      val genesis  = Seq(dApp1, dApp2, dApp3, dApp4, dApp5).map(acc => TxHelpers.genesis(acc.toAddress))
      val setDApp1 = TxHelpers.setScript(dApp1, dApp(dApp1.toAddress))
      val setDApp2 = TxHelpers.setScript(dApp2, dApp(dApp3.toAddress, secondReentrantInvoke = Some(dApp5.toAddress)))
      val setDApp3 = TxHelpers.setScript(dApp3, dApp(dApp4.toAddress, sendEnd = true, secondNextDApp = Some(dApp2.toAddress)))
      val setDApp4 = TxHelpers.setScript(dApp4, dApp(dApp2.toAddress))
      val setDApp5 = TxHelpers.setScript(dApp5, dApp(dApp3.toAddress, sendChangeDApp = true))
      val invoke = TxHelpers.invoke(
        dApp2.toAddress,
        func = fc.map(_.function.funcName),
        args = fc.map(_.args).toList.flatten,
        invoker = dApp1,
        version = TxVersion.V1
      )

      (genesis :+ setDApp1 :+ setDApp2 :+ setDApp3 :+ setDApp4 :+ setDApp5, invoke)
    }

    val (preparingTxs, invoke) = preconditions
    assertDiffAndState(
      Seq(TestBlock.create(preparingTxs)),
      TestBlock.create(Seq(invoke)),
      features()
    ) { case (snapshot, _) =>
      snapshot.errorMessage(invoke.id.value()) shouldBe None
      inside(snapshot.scriptResults.toSeq) { case Seq((_, call1)) =>
        inside(call1.invokes) { case Seq(call21, call22) =>
          inside(call21.stateChanges.invokes) { case Seq(call31) =>
            call31.stateChanges.error shouldBe empty
            call31.stateChanges.invokes shouldBe empty
          }
          inside(call22.stateChanges.invokes) { case Seq(call32) =>
            inside(call32.stateChanges.invokes) { case Seq(call42) =>
              call42.stateChanges.error shouldBe empty
              call42.stateChanges.invokes shouldBe empty
            }
          }
        }
      }
    }
  }

  private def features(invokeExpression: Boolean = false): FunctionalitySettings = {
    if (invokeExpression) {
      ContinuationTransaction.blockchainSettings.functionalitySettings
    } else {
      RideV6.blockchainSettings.functionalitySettings
    }
  }
}

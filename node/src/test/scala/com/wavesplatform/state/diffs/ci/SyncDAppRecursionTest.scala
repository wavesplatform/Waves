package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BOOLEAN
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.TxHelpers

class SyncDAppRecursionTest extends PropSpec with WithDomain {
  // A -> A -> B -> B
  property("dApp calls itself that calls other dApp that calls itself - allowed") {
    val dApp1 = TxHelpers.signer(1)
    val dApp2 = TxHelpers.signer(2)

    val balances = AddrWithBalance.enoughBalances(dApp1, dApp2)

    withDomain(DomainPresets.mostRecent, balances) { d =>
      d.helpers.setScript(dApp1, generateScript(dApp2.toAddress))
      d.helpers.setScript(dApp2, generateScript(dApp2.toAddress, sendEnd = true))

      val invoke = TxHelpers.invoke(dApp1.toAddress, Some("default"), (1 to 4).map(_ => CONST_BOOLEAN(false)), invoker = dApp1)
      d.createDiff(invoke).scriptsRun shouldBe 3
      d.appendAndAssertSucceed(invoke)
    }
  }

  // A -> B -> C -> A
  property("dApp calls dApp chain with itself at the end - prohibited") {
    val dApp1 = TxHelpers.signer(1)
    val dApp2 = TxHelpers.signer(2)
    val dApp3 = TxHelpers.signer(3)

    val balances = AddrWithBalance.enoughBalances(dApp1, dApp2, dApp3)

    withDomain(DomainPresets.mostRecent, balances) { d =>
      d.helpers.setScript(dApp1, generateScript(dApp1.toAddress))
      d.helpers.setScript(dApp2, generateScript(dApp3.toAddress))
      d.helpers.setScript(dApp3, generateScript(dApp1.toAddress))

      val invoke = TxHelpers.invoke(dApp2.toAddress, Some("default"), (1 to 4).map(_ => CONST_BOOLEAN(false)), invoker = dApp1)
      d.createDiffE(invoke) should produce(
        s"The invocation stack contains multiple invocations of the dApp at address ${dApp1.toAddress} with invocations of another dApp between them"
      )
    }
  }

  // 2 scenarios:
  // A -> B -> C -> B
  // A -> B -> C -[r]-> B
  property("calling dApp is called again - prohibited") {
    def assert(reentrant: Boolean): Unit = {
      val dApp1 = TxHelpers.signer(1)
      val dApp2 = TxHelpers.signer(2)
      val dApp3 = TxHelpers.signer(3)

      val balances = AddrWithBalance.enoughBalances(dApp1, dApp2, dApp3)

      withDomain(DomainPresets.mostRecent, balances) { d =>
        d.helpers.setScript(dApp1, generateScript(dApp1.toAddress))
        d.helpers.setScript(dApp2, generateScript(dApp3.toAddress))
        d.helpers.setScript(dApp3, generateScript(dApp2.toAddress, reentrant = reentrant))

        val invoke = TxHelpers.invoke(dApp2.toAddress, Some("default"), (1 to 4).map(_ => CONST_BOOLEAN(false)), invoker = dApp1)
        d.createDiffE(invoke) should produce(
          s"The invocation stack contains multiple invocations of the dApp at address ${dApp2.toAddress} with invocations of another dApp between them"
        )
      }
    }

    assert(reentrant = false)
    assert(reentrant = true)
  }

  // A -> B -> B -[r]-> C -[r]-> B
  property("calling twice dApp is called again after reentrant calls - prohibited") {
    val dApp1 = TxHelpers.signer(1)
    val dApp2 = TxHelpers.signer(2)
    val dApp3 = TxHelpers.signer(3)

    val balances = AddrWithBalance.enoughBalances(dApp1, dApp2, dApp3)

    withDomain(DomainPresets.mostRecent, balances) { d =>
      d.helpers.setScript(dApp1, generateScript(dApp1.toAddress))
      d.helpers
        .setScript(dApp2, generateScript(dApp2.toAddress, secondNextDApp = Some(dApp3.toAddress), sendChangeDApp = true, sendForceReentrant = true))
      d.helpers.setScript(dApp3, generateScript(dApp2.toAddress, reentrant = true, sendEnd = true))

      val invoke = TxHelpers.invoke(dApp2.toAddress, Some("default"), (1 to 4).map(_ => CONST_BOOLEAN(false)), invoker = dApp1)
      d.createDiffE(invoke) should produce(
        s"The invocation stack contains multiple invocations of the dApp at address ${dApp2.toAddress} with invocations of another dApp between them"
      )
    }
  }

  // A -> B -> C -> D -> C
  property("dApp from chain is called again - prohibited") {
    val dApp1 = TxHelpers.signer(1)
    val dApp2 = TxHelpers.signer(2)
    val dApp3 = TxHelpers.signer(3)
    val dApp4 = TxHelpers.signer(4)

    val balances = AddrWithBalance.enoughBalances(dApp1, dApp2, dApp3, dApp4)

    withDomain(DomainPresets.mostRecent, balances) { d =>
      d.helpers.setScript(dApp1, generateScript(dApp1.toAddress))
      d.helpers.setScript(dApp2, generateScript(dApp3.toAddress))
      d.helpers.setScript(dApp3, generateScript(dApp4.toAddress))
      d.helpers.setScript(dApp4, generateScript(dApp3.toAddress))

      val invoke = TxHelpers.invoke(dApp2.toAddress, Some("default"), (1 to 4).map(_ => CONST_BOOLEAN(false)), invoker = dApp1)
      d.createDiffE(invoke) should produce(
        s"The invocation stack contains multiple invocations of the dApp at address ${dApp3.toAddress} with invocations of another dApp between them"
      )
    }
  }

  // A -> B -> C -[r]-> D -> C
  property("dApp is called after reentrant call - allowed") {
    val dApp1 = TxHelpers.signer(1)
    val dApp2 = TxHelpers.signer(2)
    val dApp3 = TxHelpers.signer(3)
    val dApp4 = TxHelpers.signer(4)

    val balances = AddrWithBalance.enoughBalances(dApp1, dApp2, dApp3, dApp4)

    withDomain(DomainPresets.mostRecent, balances) { d =>
      d.helpers.setScript(dApp1, generateScript(dApp1.toAddress))
      d.helpers.setScript(dApp2, generateScript(dApp3.toAddress))
      d.helpers.setScript(dApp3, generateScript(dApp4.toAddress, reentrant = true))
      d.helpers.setScript(dApp4, generateScript(dApp3.toAddress, sendEnd = true))

      val invoke = TxHelpers.invoke(dApp2.toAddress, Some("default"), (1 to 4).map(_ => CONST_BOOLEAN(false)), invoker = dApp1)
      d.createDiff(invoke).scriptsRun shouldBe 4
      d.appendAndAssertSucceed(invoke)
    }
  }

  // A -> B -[r]-> C -> D -> B
  property("dApp is called after reentrant and usual call other dApp - allowed") {
    val dApp1 = TxHelpers.signer(1)
    val dApp2 = TxHelpers.signer(2)
    val dApp3 = TxHelpers.signer(3)
    val dApp4 = TxHelpers.signer(4)

    val balances = AddrWithBalance.enoughBalances(dApp1, dApp2, dApp3, dApp4)

    withDomain(DomainPresets.mostRecent, balances) { d =>
      d.helpers.setScript(dApp1, generateScript(dApp1.toAddress))
      d.helpers.setScript(dApp2, generateScript(dApp3.toAddress, reentrant = true))
      d.helpers.setScript(dApp3, generateScript(dApp4.toAddress))
      d.helpers.setScript(dApp4, generateScript(dApp2.toAddress, sendEnd = true))

      val invoke = TxHelpers.invoke(dApp2.toAddress, Some("default"), (1 to 4).map(_ => CONST_BOOLEAN(false)), invoker = dApp1)
      d.createDiff(invoke).scriptsRun shouldBe 4
      d.appendAndAssertSucceed(invoke)
    }
  }

  // A -> B -[r]-> C -> B -> B
  property("dApp is called from itself after reentrant call - allowed") {
    val dApp1 = TxHelpers.signer(1)
    val dApp2 = TxHelpers.signer(2)
    val dApp3 = TxHelpers.signer(3)

    val balances = AddrWithBalance.enoughBalances(dApp1, dApp2, dApp3)

    withDomain(DomainPresets.mostRecent, balances) { d =>
      d.helpers.setScript(dApp1, generateScript(dApp1.toAddress))
      d.helpers.setScript(dApp2, generateScript(dApp3.toAddress, reentrant = true, secondNextDApp = Some(dApp2.toAddress), sendEndToNext = true))
      d.helpers.setScript(dApp3, generateScript(dApp2.toAddress, sendChangeDApp = true))

      val invoke = TxHelpers.invoke(dApp2.toAddress, Some("default"), (1 to 4).map(_ => CONST_BOOLEAN(false)), invoker = dApp1)
      d.createDiff(invoke).scriptsRun shouldBe 4
      d.appendAndAssertSucceed(invoke)
    }
  }

  // A -> B -[r]-> C -> D -> C
  property("dApp that called from reentrant dApp is called again - prohibited") {
    val dApp1 = TxHelpers.signer(1)
    val dApp2 = TxHelpers.signer(2)
    val dApp3 = TxHelpers.signer(3)
    val dApp4 = TxHelpers.signer(4)

    val balances = AddrWithBalance.enoughBalances(dApp1, dApp2, dApp3, dApp4)

    withDomain(DomainPresets.mostRecent, balances) { d =>
      d.helpers.setScript(dApp1, generateScript(dApp1.toAddress))
      d.helpers.setScript(dApp2, generateScript(dApp3.toAddress, reentrant = true))
      d.helpers.setScript(dApp3, generateScript(dApp4.toAddress))
      d.helpers.setScript(dApp4, generateScript(dApp3.toAddress, sendEnd = true))

      val invoke = TxHelpers.invoke(dApp2.toAddress, Some("default"), (1 to 4).map(_ => CONST_BOOLEAN(false)), invoker = dApp1)
      d.createDiffE(invoke) should produce(
        s"The invocation stack contains multiple invocations of the dApp at address ${dApp3.toAddress} with invocations of another dApp between them"
      )
    }
  }

  // A -> B -[r]-> C -> D -> B -[r]-> E -> B
  property("dApp is called after 2 reentrant call - allowed") {
    val dApp1 = TxHelpers.signer(1)
    val dApp2 = TxHelpers.signer(2)
    val dApp3 = TxHelpers.signer(3)
    val dApp4 = TxHelpers.signer(4)
    val dApp5 = TxHelpers.signer(5)

    val balances = AddrWithBalance.enoughBalances(dApp1, dApp2, dApp3, dApp4, dApp5)

    withDomain(DomainPresets.mostRecent, balances) { d =>
      d.helpers.setScript(dApp1, generateScript(dApp1.toAddress))
      d.helpers.setScript(dApp2, generateScript(dApp3.toAddress, reentrant = true, secondNextDApp = Some(dApp5.toAddress)))
      d.helpers.setScript(dApp3, generateScript(dApp4.toAddress))
      d.helpers.setScript(dApp4, generateScript(dApp2.toAddress, sendChangeDApp = true))
      d.helpers.setScript(dApp5, generateScript(dApp2.toAddress, sendEnd = true))

      val invoke = TxHelpers.invoke(dApp2.toAddress, Some("default"), (1 to 4).map(_ => CONST_BOOLEAN(false)), invoker = dApp1)
      d.createDiff(invoke).scriptsRun shouldBe 6
      d.appendAndAssertSucceed(invoke)
    }
  }

  // A -> B -[r]-> C -> D -> B -> E -> B
  property("dApp is called after reentrant and simple call - prohibited") {
    val dApp1 = TxHelpers.signer(1)
    val dApp2 = TxHelpers.signer(2)
    val dApp3 = TxHelpers.signer(3)
    val dApp4 = TxHelpers.signer(4)
    val dApp5 = TxHelpers.signer(5)

    val balances = AddrWithBalance.enoughBalances(dApp1, dApp2, dApp3, dApp4, dApp5)

    withDomain(DomainPresets.mostRecent, balances) { d =>
      d.helpers.setScript(dApp1, generateScript(dApp1.toAddress))
      d.helpers.setScript(dApp2, generateScript(dApp3.toAddress, reentrant = true, secondNextDApp = Some(dApp5.toAddress)))
      d.helpers.setScript(dApp3, generateScript(dApp4.toAddress))
      d.helpers.setScript(dApp4, generateScript(dApp2.toAddress, sendChangeDApp = true, sendForceInvoke = true))
      d.helpers.setScript(dApp5, generateScript(dApp2.toAddress, sendEnd = true))

      val invoke = TxHelpers.invoke(dApp2.toAddress, Some("default"), (1 to 4).map(_ => CONST_BOOLEAN(false)), invoker = dApp1)
      d.createDiffE(invoke) should produce(
        s"The invocation stack contains multiple invocations of the dApp at address ${dApp2.toAddress} with invocations of another dApp between them"
      )
    }
  }

  // A -> B -> C -> D
  //        -[r]-> E -> C -> B
  property("dApps (B - reentrant, C - simple) are called after calls from new chain - allowed") {
    val dApp1 = TxHelpers.signer(1)
    val dApp2 = TxHelpers.signer(2)
    val dApp3 = TxHelpers.signer(3)
    val dApp4 = TxHelpers.signer(4)
    val dApp5 = TxHelpers.signer(5)

    val balances = AddrWithBalance.enoughBalances(dApp1, dApp2, dApp3, dApp4, dApp5)

    withDomain(DomainPresets.mostRecent, balances) { d =>
      d.helpers.setScript(dApp1, generateScript(dApp1.toAddress))
      d.helpers.setScript(dApp2, generateScript(dApp3.toAddress, secondReentrantInvoke = Some(dApp5.toAddress)))
      d.helpers.setScript(dApp3, generateScript(dApp4.toAddress, sendEnd = true, secondNextDApp = Some(dApp2.toAddress)))
      d.helpers.setScript(dApp4, generateScript(dApp2.toAddress))
      d.helpers.setScript(dApp5, generateScript(dApp3.toAddress, sendChangeDApp = true))

      val invoke = TxHelpers.invoke(dApp2.toAddress, Some("default"), (1 to 4).map(_ => CONST_BOOLEAN(false)), invoker = dApp1)
      d.createDiff(invoke).scriptsRun shouldBe 6
      d.appendAndAssertSucceed(invoke)
    }
  }

  private[this] def generateScript(
      nextDApp: Address,
      sendEnd: Boolean = false,
      reentrant: Boolean = false,
      secondNextDApp: Option[Address] = None,
      sendEndToNext: Boolean = false,
      sendChangeDApp: Boolean = false,
      sendForceInvoke: Boolean = false,
      sendForceReentrant: Boolean = false,
      secondReentrantInvoke: Option[Address] = None
  ): Script = TestCompiler(V5).compileContract {
    val func = if (reentrant) "reentrantInvoke" else "invoke"
    val args = s"[sendEnd, $sendChangeDApp, $sendForceInvoke, $sendForceReentrant]"
    s"""
       | {-# STDLIB_VERSION 5       #-}
       | {-# CONTENT_TYPE   DAPP    #-}
       | {-# SCRIPT_TYPE    ACCOUNT #-}
       |
       | @Callable(i)
       | func default(end: Boolean, useSecondAddress: Boolean, forceInvoke: Boolean, forceReentrant: Boolean) =
       |    if (end)
       |      then
       |        []
       |      else {
       |        let endWithNextDApp = useSecondAddress && $sendEndToNext
       |        let sendEnd = $sendEnd || endWithNextDApp
       |        let address = ${secondNextDApp.fold("")(a => s"if (useSecondAddress) then Address(base58'$a') else")} Address(base58'$nextDApp')
       |        strict r =
       |          if (forceInvoke || endWithNextDApp) # param 'endWithNextDApp' is used for self-recursion check without reentrancy
       |            then
       |              invoke(address, "default", $args, [])
       |            else if (forceReentrant)
       |              then
       |                reentrantInvoke(address, "default", $args, [])
       |              else
       |                $func(address, "default", $args, [])
       |
       |        ${secondReentrantInvoke.fold("")(a => s""" strict r2 = reentrantInvoke(Address(base58'$a'), "default", $args, []) """)}
       |        []
       |      }
     """.stripMargin
  }
}

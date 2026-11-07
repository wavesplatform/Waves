package com.wavesplatform.state.diffs.ci

import com.google.common.primitives.Ints
import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.{InvokeScriptResult, NewTransactionInfo, StateSnapshot, TxMeta}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.TxHelpers.*

class InvokeAffectedAddressTest extends PropSpec with WithDomain {
  import DomainPresets.*

  private def dApp(failed: Boolean) =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = [${if (failed) "Burn(base58'', 1)" else ""}]
       """.stripMargin
    )

  property("tx belongs to dApp address without actions") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      Seq(true, false).foreach { failed =>
        d.appendBlock(setScript(secondSigner, dApp(failed)))
        if (failed)
          d.appendAndAssertFailed(invoke(secondAddress))
        else
          d.appendAndAssertSucceed(invoke(secondAddress))
        d.liquidSnapshot.transactions.head._2.affected shouldBe Set(defaultAddress, secondAddress)
      }
    }
  }

  property("tx belongs to dApp address when called by alias") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      d.appendBlock(createAlias("alias", secondSigner))
      Seq(true, false).foreach { failed =>
        d.appendBlock(setScript(secondSigner, dApp(failed)))
        if (failed)
          d.appendAndAssertFailed(invoke(Alias.create("alias").explicitGet()))
        else
          d.appendAndAssertSucceed(invoke(Alias.create("alias").explicitGet()))
        d.liquidSnapshot.transactions.head._2.affected shouldBe Set(defaultAddress, secondAddress)
      }
    }
  }

  property("tx belongs to dApp address when called by alias created in current block") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      Seq(true, false).foreach { failed =>
        val invokeTx = invoke(Alias.create(s"$failed").explicitGet())
        val aliasTx  = createAlias(s"$failed", secondSigner)
        d.appendBlock(setScript(secondSigner, dApp(failed)))
        if (failed) {
          d.appendBlock(aliasTx, invokeTx)
          d.liquidSnapshot.errorMessage(invokeTx.id()) shouldBe defined
        } else
          d.appendAndAssertSucceed(aliasTx, invokeTx)
        d.liquidSnapshot.transactions(invokeTx.id()).affected shouldBe Set(defaultAddress, secondAddress)
      }
    }
  }

  property("tx belongs to zero-amount ScriptTransfer recipient of a nested call") {
    val dApp2     = signer(2)
    val recipient = signer(3).toAddress
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner, dApp2)) { d =>
      val caller = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = Address(base58'${dApp2.toAddress}').invoke("default", [], [])
           |   []
           | }
         """.stripMargin
      )
      val callee = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = [ScriptTransfer(Address(base58'$recipient'), 0, unit)]
         """.stripMargin
      )
      val invokeTx = invoke(secondAddress)
      d.appendBlock(setScript(secondSigner, caller), setScript(dApp2, callee))
      d.appendAndAssertSucceed(invokeTx)
      d.liquidSnapshot.balances.keySet.map(_._1) should not contain recipient
      d.liquidSnapshot.transactions(invokeTx.id()).affected shouldBe Set(defaultAddress, secondAddress, dApp2.toAddress, recipient)
    }
  }

  property("failed tx does not belong to ScriptTransfer recipient of a nested call") {
    val dApp2     = signer(2)
    val recipient = signer(3).toAddress
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner, dApp2)) { d =>
      val caller = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = Address(base58'${dApp2.toAddress}').invoke("default", [], [])
           |   [Burn(base58'', 1)]
           | }
         """.stripMargin
      )
      val callee = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = [ScriptTransfer(Address(base58'$recipient'), 1, unit)]
         """.stripMargin
      )
      val invokeTx = invoke(secondAddress)
      d.appendBlock(setScript(secondSigner, caller), setScript(dApp2, callee))
      d.appendAndAssertFailed(invokeTx)
      d.liquidSnapshot.transactions(invokeTx.id()).affected shouldBe Set(defaultAddress, secondAddress, dApp2.toAddress)
    }
  }

  property("tx belongs to recipient of a lease cancelled in the same call") {
    val recipient = signer(3).toAddress
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   let lease = Lease(Address(base58'$recipient'), 1)
           |   [lease, LeaseCancel(calculateLeaseId(lease))]
           | }
         """.stripMargin
      )
      val invokeTx = invoke(secondAddress)
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendAndAssertSucceed(invokeTx)
      d.liquidSnapshot.transactions(invokeTx.id()).affected shouldBe Set(defaultAddress, secondAddress, recipient)
    }
  }

  property("collecting addresses of a deep call chain does not overflow the stack") {
    withDomain(RideV5) { d =>
      val depth = 100000
      val dApps = (1 to depth).map(i => Address.fromPublicKey(PublicKey(ByteStr(Ints.toByteArray(i) ++ new Array[Byte](28)))))
      val result = dApps.foldRight(InvokeScriptResult.empty) { (dApp, nested) =>
        InvokeScriptResult(invokes = Seq(InvokeScriptResult.Invocation(dApp, InvokeScriptResult.Call("f", Nil), Nil, nested)))
      }
      val tx       = invoke(secondAddress)
      val snapshot = StateSnapshot(scriptResults = Map(tx.id() -> result))
      NewTransactionInfo.create(tx, TxMeta.Status.Succeeded, snapshot, d.blockchain).affected should contain allElementsOf dApps
    }
  }
}

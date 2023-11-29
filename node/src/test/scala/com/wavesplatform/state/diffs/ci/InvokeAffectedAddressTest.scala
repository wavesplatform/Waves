package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Alias
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
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
}

package com.wavesplatform.state.diffs.ci
import com.wavesplatform.account.Alias
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.TxHelpers._

class InvokeAffectedAddressTest extends PropSpec with WithDomain {
  import DomainPresets._

  property("tx belongs to dApp address without actions") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = []
         """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendAndAssertSucceed(invoke(secondAddress))
      d.liquidDiff.transactions.head._2.affected shouldBe Set(defaultAddress, secondAddress)
    }
  }

  property("tx belongs to dApp address when called by alias") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = []
         """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlock(createAlias("alias", secondSigner))
      d.appendAndAssertSucceed(invoke(Alias.create("alias").explicitGet()))
      d.liquidDiff.transactions.head._2.affected shouldBe Set(defaultAddress, secondAddress)
    }
  }

  property("tx belongs to dApp address when called by alias created in current block") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = []
         """.stripMargin
      )
      val invokeTx = invoke(Alias.create("alias").explicitGet())
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendAndAssertSucceed(createAlias("alias", secondSigner), invokeTx)
      d.liquidDiff.transactions(invokeTx.id()).affected shouldBe Set(defaultAddress, secondAddress)
    }
  }
}

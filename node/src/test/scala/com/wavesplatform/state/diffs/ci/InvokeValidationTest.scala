package com.wavesplatform.state.diffs.ci
import com.wavesplatform.TestValues.invokeFee
import com.wavesplatform.account.Alias
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.StdLibVersion.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test.{PropSpec, produce}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers._
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment

class InvokeValidationTest extends PropSpec with WithDomain {
  import DomainPresets._

  property("invoke by alias with wrong chainId") {
    withDomain(RideV5) { d =>
      val alias = Alias.fromString("alias:X:alias").explicitGet()
      d.appendBlockE(invoke(alias)) should produce("Data from other network: expected: 84(T), actual: 88(X)")
    }
  }

  property("invoke unexisting function") {
    withDomain(RideV5, Seq(AddrWithBalance(secondAddress, ENOUGH_AMT))) { d =>
      val script = TestCompiler(V5).compileContract(
        """
          | @Callable(i)
          | func f() = []
        """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, script))
      d.appendBlockE(invoke(secondAddress, Some("g"))) should produce("Cannot find callable function `g`")
    }
  }

  property("invoke address without script") {
    withDomain(RideV5) { d =>
      d.appendBlockE(invoke(secondAddress)) should produce(s"No contract at address $secondAddress")
    }
  }

  property("invoke address with set expression") {
    withDomain(RideV5, Seq(AddrWithBalance(secondAddress, ENOUGH_AMT))) { d =>
      val script = TestCompiler(V5).compileExpression("true")
      d.appendBlock(setScript(secondSigner, script))
      d.appendBlockE(invoke(secondAddress)) should produce("Trying to call dApp on the account with expression script")
    }
  }

  property("invoke payment balance should be checked before script execution only if spending exceeds fee") {
    withDomain(RideV5, Seq(AddrWithBalance(secondAddress, ENOUGH_AMT), AddrWithBalance(signer(2).toAddress, invokeFee))) { d =>
      val script = TestCompiler(V5).compileContract(
        """
          | @Callable(i)
          | func default() = if (true) then throw() else []
        """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, script))
      d.appendBlockE(invoke(secondAddress, invoker = signer(2), payments = Seq(Payment(invokeFee, Waves)))) should produce(
        "Explicit script termination"
      )
      d.appendBlockE(invoke(secondAddress, invoker = signer(2), payments = Seq(Payment(invokeFee + 1, Waves)))) should produce(
        "Attempt to transfer unavailable funds: " +
          "Transaction application leads to negative waves balance to (at least) temporary negative state, " +
          "current balance equals 500000, spends equals -1000001, result is -500001"
      )
    }
  }
}

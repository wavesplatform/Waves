package com.wavesplatform.state.diffs.ci
import com.wavesplatform.TestValues.invokeFee
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.StdLibVersion.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.TxHelpers.{invoke, secondSigner, setScript}

class InvokeFeeTest extends PropSpec with WithDomain {
  import DomainPresets._

  property("invoke standard fee") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        """
          | @Callable(i)
          | func default() = []
        """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlock(invoke(fee = invokeFee))
      d.appendBlockE(invoke(fee = invokeFee - 1)) should produce(
        "Fee in WAVES for InvokeScriptTransaction (499999 in WAVES) does not exceed minimal value of 500000 WAVES"
      )
    }
  }
}

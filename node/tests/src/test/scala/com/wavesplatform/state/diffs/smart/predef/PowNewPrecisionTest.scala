package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.{TxHelpers, TxVersion}

class PowNewPrecisionTest extends PropSpec with WithDomain {

  private val contract = TestCompiler(V4).compileContract(
    """
      | @Callable(i)
      | func default() = {
      |   let digits8 = 8
      |   let alpha = 50
      |   let alphaDigits = 2
      |   let beta = 46000000
      |   let scale8 = 100000000
      |   let scale12 = 1000000000000
      |   let x = 2661956191736
      |   let y = 2554192264270
      |   let sk = (((fraction(scale12, x, y) + fraction(scale12, y, x)) / 2) / 10000)
      |   let r1 = (fraction((x + y), scale8, pow(sk, digits8, alpha, alphaDigits, digits8, CEILING)) + (2 * fraction(pow(fraction(x, y, scale8), 0, 5, 1, (digits8 / 2), DOWN), pow((sk - beta), digits8, alpha, alphaDigits, digits8, DOWN), scale8)))
      |   let r2 = pow(10, 6, 6, 0, 0, CEILING)
      |   [
      |     IntegerEntry("result1", r1),
      |     IntegerEntry("result2", r2)
      |   ]
      | }
    """.stripMargin
  )

  private val scenario = {
    val master  = TxHelpers.signer(0)
    val invoker = TxHelpers.signer(1)

    val balances = AddrWithBalance.enoughBalances(master, invoker)

    val setScript = TxHelpers.setScript(master, contract)
    val invoke    = () => TxHelpers.invoke(master.toAddress, invoker = invoker, version = TxVersion.V3)

    (balances, setScript, invoke, master.toAddress)
  }

  property("pow has bigger precision before SynchronousCalls") {
    val (balances, setScript, invoke, dApp) = scenario
    withDomain(DomainPresets.RideV4, balances) { d =>
      d.appendBlock(setScript)

      d.appendBlock(invoke())
      d.blockchain.accountData(dApp, "result1").get.value shouldBe 9049204201489L
      d.blockchain.accountData(dApp, "result2").get.value shouldBe 1
    }
  }

  property("pow changes precision after SynchronousCalls") {
    val (balances, setScript, invoke, dApp) = scenario
    withDomain(DomainPresets.RideV5.configure(_.copy(enforceTransferValidationAfter = 0)), balances) { d =>
      d.appendBlock(setScript)

      d.appendBlock(invoke())
      d.blockchain.accountData(dApp, "result1").get.value shouldBe 9049204201491L
      d.blockchain.accountData(dApp, "result2").get.value shouldBe 0
    }
  }
}

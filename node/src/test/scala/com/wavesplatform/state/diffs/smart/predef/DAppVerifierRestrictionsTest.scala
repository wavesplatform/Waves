package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test._
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.EitherValues

class DAppVerifierRestrictionsTest extends PropSpec with WithDomain with EitherValues {

  import DomainPresets._

  private def contract(call: String) = TestCompiler(V5).compileContract(
    s"""
       | {-# STDLIB_VERSION 5       #-}
       | {-# CONTENT_TYPE   DAPP    #-}
       | {-# SCRIPT_TYPE    ACCOUNT #-}
       |
       | @Verifier(tx)
       | func verifier() = {
       |   strict r = $call(Address(base58''), "default", [], [])
       |   true
       | }
     """.stripMargin
  )

  private val scenario = {
    val account1 = TxHelpers.signer(1)
    val account2 = TxHelpers.signer(2)

    val balances = AddrWithBalance.enoughBalances(account1, account2)

    val setInvoke = TxHelpers.setScript(account1, contract("invoke"))
    val setReentrantInvoke = TxHelpers.setScript(account2, contract("reentrantInvoke"))

    (balances, setInvoke, setReentrantInvoke)
  }

  property("sync calls are prohibited from dApp verifier") {
    val (balances, setInvoke, setReentrantInvoke) = scenario
    withDomain(RideV5, balances) { d =>
      (the[RuntimeException] thrownBy d.appendBlock(setInvoke)).getMessage should include(s"DApp-to-dApp invocations are not allowed from verifier")
      (the[RuntimeException] thrownBy d.appendBlock(setReentrantInvoke)).getMessage should include(
        s"DApp-to-dApp invocations are not allowed from verifier"
      )
    }
  }
}

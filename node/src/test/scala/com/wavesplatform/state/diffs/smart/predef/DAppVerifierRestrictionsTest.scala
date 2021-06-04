package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.{NoShrink, TestTime, TransactionGen}
import org.scalatest.{EitherValues, Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class DAppVerifierRestrictionsTest
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with TransactionGen
    with NoShrink
    with WithDomain
    with EitherValues {

  import DomainPresets._

  private val time = new TestTime
  private def ts   = time.getTimestamp()

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

  private val scenario =
    for {
      account1 <- accountGen
      account2 <- accountGen
      fee      <- ciFee()
      genesis1           = GenesisTransaction.create(account1.toAddress, ENOUGH_AMT, ts).explicitGet()
      genesis2           = GenesisTransaction.create(account2.toAddress, ENOUGH_AMT, ts).explicitGet()
      setInvoke          = SetScriptTransaction.selfSigned(1.toByte, account1, Some(contract("invoke")), fee, ts).explicitGet()
      setReentrantInvoke = SetScriptTransaction.selfSigned(1.toByte, account2, Some(contract("reentrantInvoke")), fee, ts).explicitGet()
    } yield (List(genesis1, genesis2), setInvoke, setReentrantInvoke)

  property("sync calls are prohibited from dApp verifier") {
    val (genesis, setInvoke, setReentrantInvoke) = scenario.sample.get
    withDomain(RideV5) { d =>
      d.appendBlock(genesis: _*)
      (the[RuntimeException] thrownBy d.appendBlock(setInvoke)).getMessage should include(s"DApp-to-dApp invocations are not allowed from verifier")
      (the[RuntimeException] thrownBy d.appendBlock(setReentrantInvoke)).getMessage should include(s"DApp-to-dApp invocations are not allowed from verifier")
    }
  }
}

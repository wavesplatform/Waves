package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.{StdLibVersion, V4, V5}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import com.wavesplatform.{NoShrink, TestTime, TransactionGen}
import org.scalatest.{EitherValues, Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CallableV5LimitTest
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

  private def contract(grothCount: Int, version: StdLibVersion) = TestCompiler(version).compileContract(
    s"""
       |{-# STDLIB_VERSION $version #-}
       |{-# CONTENT_TYPE   DAPP     #-}
       |{-#SCRIPT_TYPE     ACCOUNT  #-}
       |
       | @Callable(i)
       | func default() = {
       |   let key   = base64'mY//hEITCBCZUJUN/wsOlw1iUSSOESL6PFSbN1abGK80t5jPNICNlPuSorio4mmWpf+4uOyv3gPZe54SYGM4pfhteqJpwFQxdlpwXWyYxMTNaSLDj8VtSn/EJaSu+P6nFmWsda3mTYUPYMZzWE4hMqpDgFPcJhw3prArMThDPbR3Hx7E6NRAAR0LqcrdtsbDqu2T0tto1rpnFILdvHL4PqEUfTmF2mkM+DKj7lKwvvZUbukqBwLrnnbdfyqZJryzGAMIa2JvMEMYszGsYyiPXZvYx6Luk54oWOlOrwEKrCY4NMPwch6DbFq6KpnNSQwOpgRYCz7wpjk57X+NGJmo85tYKc+TNa1rT4/DxG9v6SHkpXmmPeHhzIIW8MOdkFjxB5o6Qn8Fa0c6Tt6br2gzkrGr1eK5/+RiIgEzVhcRrqdY/p7PLmKXqawrEvIv9QZ3ijytPNwinlC8XdRLO/YvP33PjcI9WSMcHV6POP9KPMo1rngaIPMegKgAvTEouNFKp4v3wAXRXX5xEjwXAmM5wyB/SAOaPPCK/emls9kqolHsaj7nuTTbrvSV8bqzUwzQ'
       |   let proof = base64'g53N8ecorvG2sDgNv8D7quVhKMIIpdP9Bqk/8gmV5cJ5Rhk9gKvb4F0ll8J/ZZJVqa27OyciJwx6lym6QpVK9q1ASrqio7rD5POMDGm64Iay/ixXXn+//F+uKgDXADj9AySri2J1j3qEkqqe3kxKthw94DzAfUBPncHfTPazVtE48AfzB1KWZA7Vf/x/3phYs4ckcP7ZrdVViJVLbUgFy543dpKfEH2MD30ZLLYRhw8SatRCyIJuTZcMlluEKG+d'
       |   let input = base64'aZ8tqrOeEJKt4AMqiRF/WJhIKTDC0HeDTgiJVLZ8OEs='
       |
       |   strict r = ${(1 to grothCount).map(_ => s"groth16Verify_8inputs(key, proof, input)").mkString(" && ")}
       |   []
       | }
     """.stripMargin
  )

  private def syncDAppScript(address: Address) = TestCompiler(V5).compileContract(
    s"""
       |{-# STDLIB_VERSION 5       #-}
       |{-# CONTENT_TYPE   DAPP    #-}
       |{-#SCRIPT_TYPE     ACCOUNT #-}
       |
       | @Callable(i)
       | func default() = {
       |   strict r = invoke(Address(base58'$address'), "default", [], [])
       |   []
       | }
     """.stripMargin
  )

  private val scenario =
    for {
      dApp     <- accountGen
      syncDApp <- accountGen
      invoker  <- accountGen
      fee      <- ciFee()
      gTx1                  = GenesisTransaction.create(dApp.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx2                  = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx3                  = GenesisTransaction.create(syncDApp.toAddress, ENOUGH_AMT, ts).explicitGet()
      setAcceptableScript   = SetScriptTransaction.selfSigned(1.toByte, dApp, Some(contract(5, V5)), fee, ts).explicitGet()
      setProhibitedScript   = SetScriptTransaction.selfSigned(1.toByte, dApp, Some(contract(6, V5)), fee, ts).explicitGet()
      setProhibitedV4Script = SetScriptTransaction.selfSigned(1.toByte, dApp, Some(contract(5, V4)), fee, ts).explicitGet()
      setSyncDApp           = SetScriptTransaction.selfSigned(1.toByte, syncDApp, Some(syncDAppScript(dApp.toAddress)), fee, ts).explicitGet()
      invoke                = InvokeScriptTransaction.selfSigned(TxVersion.V3, invoker, dApp.toAddress, None, Nil, fee, Waves, ts).explicitGet()
      syncInvoke            = InvokeScriptTransaction.selfSigned(TxVersion.V3, invoker, syncDApp.toAddress, None, Nil, fee, Waves, ts).explicitGet()
    } yield (Seq(gTx1, gTx2, gTx3), setAcceptableScript, setProhibitedScript, setProhibitedV4Script, setSyncDApp, invoke, syncInvoke)

  property("callable limit is 10000 from V5") {
    val (genesisTxs, setAcceptable, setProhibitedScript, setProhibitedV4Script, setSyncDApp, invoke, syncInvoke) = scenario.sample.get
    withDomain(RideV4) { d =>
      d.appendBlock(genesisTxs: _*)
      (the[RuntimeException] thrownBy d.appendBlock(setProhibitedV4Script)).getMessage should include(
        "Contract function (default) is too complex: 9528 > 4000"
      )
    }
    withDomain(RideV5) { d =>
      d.appendBlock(genesisTxs: _*)
      (the[RuntimeException] thrownBy d.appendBlock(setProhibitedV4Script)).getMessage should include(
        "Contract function (default) is too complex: 9528 > 4000"
      )
      (the[RuntimeException] thrownBy d.appendBlock(setProhibitedScript)).getMessage should include(
        "Contract function (default) is too complex: 11432 > 10000"
      )
      d.appendBlock(setAcceptable, invoke)
      d.blockchain.transactionInfo(invoke.id.value()).get._3 shouldBe true

      d.appendBlock(setSyncDApp, syncInvoke)
      d.blockchain.transactionInfo(syncInvoke.id.value()).get._3 shouldBe true
    }
  }
}

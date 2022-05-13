package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.{StdLibVersion, V4, V5}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.EitherValues
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CallableV5LimitTest extends PropSpec with ScalaCheckPropertyChecks with WithDomain with EitherValues {

  import DomainPresets.*

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

  property("callable limit is 10000 from V5") {
    val dApp     = TxHelpers.signer(0)
    val syncDApp = TxHelpers.signer(1)
    val invoker  = TxHelpers.signer(2)
    val balances = AddrWithBalance.enoughBalances(dApp, syncDApp, invoker)

    val setAcceptableScript   = TxHelpers.setScript(dApp, contract(5, V5))
    val setProhibitedScript   = TxHelpers.setScript(dApp, contract(6, V5))
    val setProhibitedV4Script = TxHelpers.setScript(dApp, contract(5, V4))
    val setSyncDApp           = TxHelpers.setScript(syncDApp, syncDAppScript(dApp.toAddress))
    val invoke                = TxHelpers.invoke(dApp.toAddress, func = None, invoker = invoker)
    val syncInvoke            = TxHelpers.invoke(syncDApp.toAddress, func = None, invoker = invoker)

    withDomain(RideV4, balances) { d =>
      d.appendBlockE(setProhibitedV4Script) should produce("Contract function (default) is too complex: 9528 > 4000")
    }
    withDomain(RideV5, balances) { d =>
      d.appendBlockE(setProhibitedV4Script) should produce("Contract function (default) is too complex: 9528 > 4000")
      d.appendBlockE(setProhibitedScript) should produce("Contract function (default) is too complex: 11432 > 10000")
      d.appendBlock(setAcceptableScript, invoke)
      d.blockchain.transactionSucceeded(invoke.id.value()) shouldBe true

      d.appendBlock(setSyncDApp, syncInvoke)
      d.blockchain.transactionSucceeded(syncInvoke.id.value()) shouldBe true
    }
  }
}

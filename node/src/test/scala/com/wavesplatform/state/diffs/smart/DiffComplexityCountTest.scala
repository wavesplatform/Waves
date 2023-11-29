package com.wavesplatform.state.diffs.smart

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.{EitherValues, Inside}

class DiffComplexityCountTest extends PropSpec with Inside with WithState with DBCacheSettings with WithDomain with EitherValues {

  private val activationHeight = 4

  private val fsWithV5 = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id    -> 0,
      BlockchainFeatures.SmartAssets.id      -> 0,
      BlockchainFeatures.Ride4DApps.id       -> 0,
      BlockchainFeatures.FeeSponsorship.id   -> 0,
      BlockchainFeatures.DataTransaction.id  -> 0,
      BlockchainFeatures.BlockReward.id      -> 0,
      BlockchainFeatures.BlockV5.id          -> 0,
      BlockchainFeatures.SynchronousCalls.id -> activationHeight
    ),
    estimatorPreCheckHeight = Int.MaxValue
  )

  // ~1900 complexity
  val groth: String =
    s"""
       | let key = base64'mY//hEITCBCZUJUN/wsOlw1iUSSOESL6PFSbN1abGK80t5jPNICNlPuSorio4mmWpf+4uOyv3gPZe54SYGM4pfhteqJpwFQxdlpwXWyYxMTNaSLDj8VtSn/EJaSu+P6nFmWsda3mTYUPYMZzWE4hMqpDgFPcJhw3prArMThDPbR3Hx7E6NRAAR0LqcrdtsbDqu2T0tto1rpnFILdvHL4PqEUfTmF2mkM+DKj7lKwvvZUbukqBwLrnnbdfyqZJryzGAMIa2JvMEMYszGsYyiPXZvYx6Luk54oWOlOrwEKrCY4NMPwch6DbFq6KpnNSQwOpgRYCz7wpjk57X+NGJmo85tYKc+TNa1rT4/DxG9v6SHkpXmmPeHhzIIW8MOdkFjxB5o6Qn8Fa0c6Tt6br2gzkrGr1eK5/+RiIgEzVhcRrqdY/p7PLmKXqawrEvIv9QZ3ijytPNwinlC8XdRLO/YvP33PjcI9WSMcHV6POP9KPMo1rngaIPMegKgAvTEouNFKp4v3wAXRXX5xEjwXAmM5wyB/SAOaPPCK/emls9kqolHsaj7nuTTbrvSV8bqzUwzQ'
       | let proof = base64'g53N8ecorvG2sDgNv8D7quVhKMIIpdP9Bqk/8gmV5cJ5Rhk9gKvb4F0ll8J/ZZJVqa27OyciJwx6lym6QpVK9q1ASrqio7rD5POMDGm64Iay/ixXXn+//F+uKgDXADj9AySri2J1j3qEkqqe3kxKthw94DzAfUBPncHfTPazVtE48AfzB1KWZA7Vf/x/3phYs4ckcP7ZrdVViJVLbUgFy543dpKfEH2MD30ZLLYRhw8SatRCyIJuTZcMlluEKG+d'
       | let input = base64'aZ8tqrOeEJKt4AMqiRF/WJhIKTDC0HeDTgiJVLZ8OEs='
       | groth16Verify_8inputs(key, proof, input)
    """.stripMargin

  private val verifier: Script = {
    val script = s"""
                    | {-# STDLIB_VERSION 4        #-}
                    | {-# SCRIPT_TYPE ASSET       #-}
                    | {-# CONTENT_TYPE EXPRESSION #-}
                    |
                    | if (true)
                    |   then true
                    |   else ($groth)
                    |
                  """.stripMargin
    ScriptCompiler.compile(script, ScriptEstimatorV3(fixOverflow = true, overhead = true)).explicitGet()._1
  }

  private def dApp(asset: IssuedAsset): Script = TestCompiler(V4).compileContract(
    s"""
       | {-# STDLIB_VERSION 4       #-}
       | {-# CONTENT_TYPE   DAPP    #-}
       | {-# SCRIPT_TYPE    ACCOUNT #-}
       |
       | @Callable(i)
       | func default() = {
       |   strict cond =
       |     if (true)
       |       then true
       |       else ($groth)
       |
       |   [
       |     ScriptTransfer(i.caller, 1, base58'$asset'),
       |     Burn(base58'$asset', 1),
       |     Reissue(base58'$asset', 1, true)
       |   ]
       | }
       """.stripMargin
  )

  private val paymentPreconditions = {
    val account1 = TxHelpers.signer(0)
    val account2 = TxHelpers.signer(1)

    val balances = AddrWithBalance.enoughBalances(account1, account2)

    val issue       = TxHelpers.issue(account1, ENOUGH_AMT, script = Some(verifier))
    val asset       = IssuedAsset(issue.id())
    val transfer1   = TxHelpers.transfer(account1, account2.toAddress, amount = Int.MaxValue, asset = asset)
    val setVerifier = TxHelpers.setScript(account2, verifier)
    val setDApp     = TxHelpers.setScript(account1, dApp(asset))

    val payments           = Seq(Payment(1, asset), Payment(1, asset))
    val invokeFromScripted = () => TxHelpers.invoke(account1.toAddress, invoker = account2, payments = payments, fee = TxHelpers.ciFee(6))

    (balances, Seq(issue, transfer1, setVerifier, setDApp), invokeFromScripted)
  }

  property(s"evaluated complexity is used instead of estimated one after activation ${BlockchainFeatures.SynchronousCalls}") {
    val (balances, preparingTxs, invoke) = paymentPreconditions
    withDomain(domainSettingsWithFS(fsWithV5), balances) { d =>
      d.appendBlock(preparingTxs*)

      val invoke1 = invoke()
      d.appendBlock(invoke1)
      d.liquidSnapshot.errorMessage(invoke1.id()) shouldBe empty
      d.liquidSnapshot.scriptsComplexity shouldBe 11459 // 3 actions + 2 payments + verifier = 6 * 1900 = 11400
      // for dApp evaluated complexity is always used after implementation of the snapshots

      d.appendBlock()
      d.blockchainUpdater.height shouldBe activationHeight

      val invoke2 = invoke()
      d.appendBlock(invoke2)
      d.liquidSnapshot.errorMessage(invoke2.id()) shouldBe empty
      d.liquidSnapshot.scriptsComplexity shouldBe 17
    }
  }
}

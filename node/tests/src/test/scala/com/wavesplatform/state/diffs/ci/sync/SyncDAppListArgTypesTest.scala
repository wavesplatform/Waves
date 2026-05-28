package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.TransactionGenBase
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures.*
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.evaluator.ctx.impl.GlobalValNames
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.test.*
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.{GenesisTransaction, Transaction, TxHelpers, TxVersion}

class SyncDAppListArgTypesTest extends PropSpec with WithDomain with TransactionGenBase {
  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private def dApp1Script(dApp2: Address, args: String): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |    strict r = Address(base58'$dApp2').invoke("default", [$args], [])
         |    []
         | }
       """.stripMargin
    )

  private val dApp2Script: Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default(a: Int) = []
       """.stripMargin
    )

  private def scenario(args: String): (Seq[Transaction], () => InvokeScriptTransaction) = {
    val invoker  = accountGen.sample.get
    val dApp1    = accountGen.sample.get
    val dApp2    = accountGen.sample.get
    val fee      = ciFee().sample.get
    val gTxs     = Seq(invoker, dApp1, dApp2).map(acc => GenesisTransaction.create(acc.toAddress, ENOUGH_AMT, ts).explicitGet())
    val ssTx1    = TxHelpers.setScript(dApp1, dApp1Script(dApp2.toAddress, args), fee, 1.toByte)
    val ssTx2    = TxHelpers.setScript(dApp2, dApp2Script, fee, 1.toByte)
    val invokeTx = () => TxHelpers.invoke(dApp1.toAddress, invoker = invoker, fee = fee, version = TxVersion.V3, timestamp = ts)
    (gTxs ++ Seq(ssTx1, ssTx2), invokeTx)
  }

  private val settings =
    TestFunctionalitySettings.Enabled
      .copy(preActivatedFeatures = Map(Ride4DApps.id -> 0, BlockV5.id -> 0, SynchronousCalls.id -> 0, RideV6.id -> 3))

  private def assert(forbidAfterActivation: Boolean, args: String) = {
    withDomain(domainSettingsWithFS(settings)) { d =>
      val (preparingTxs, invoke) = scenario(args)
      d.appendBlock(preparingTxs*)

      val invoke1 = invoke()
      d.appendBlock(invoke1)
      d.blockchain.transactionSucceeded(invoke1.id.value()) shouldBe true

      val invoke2 = invoke()
      if (forbidAfterActivation) {
        (the[Exception] thrownBy d.appendBlock(invoke2)).getMessage should include(
          s"All arguments of InvokeScript must be one of the types: List[], Boolean, Int, ByteVector, String"
        )
      } else {
        d.appendBlock(invoke2)
        d.blockchain.transactionSucceeded(invoke2.id.value()) shouldBe true
      }
    }
  }

  property("sync call args types check") {
    assert(forbidAfterActivation = false, "1")
    assert(forbidAfterActivation = false, """ "s" """)
    assert(forbidAfterActivation = false, "true")
    assert(forbidAfterActivation = false, "base58''")
    assert(forbidAfterActivation = false, """ [1, "s", true, base58''] """)
    assert(forbidAfterActivation = false, """ [] """)

    assert(forbidAfterActivation = true, GlobalValNames.Unit)
    assert(forbidAfterActivation = true, "toBigInt(1)")
    assert(forbidAfterActivation = true, "[toBigInt(1)]")
    assert(forbidAfterActivation = true, "[unit]")
    assert(forbidAfterActivation = true, "[[]]")
  }
}

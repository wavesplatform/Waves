package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.TestTime
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}

class SyncDAppBalanceCheckTest extends PropSpec with WithDomain {
  import DomainPresets._

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private def dApp1Script(dApp2: Address): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |    strict r = Address(base58'$dApp2').invoke("default", [], [AttachedPayment(unit, 100)])
         |    []
         | }
       """.stripMargin
    )

  private val dApp2Script: Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() =
         |   [
         |     ScriptTransfer(i.caller, 100, unit)
         |   ]
       """.stripMargin
    )

  private val scenario =
    for {
      invoker <- accountGen
      dApp1   <- accountGen
      dApp2   <- accountGen
      fee     <- ciFee()
      gTx1     = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx2     = GenesisTransaction.create(dApp1.toAddress, fee, ts).explicitGet()
      gTx3     = GenesisTransaction.create(dApp2.toAddress, ENOUGH_AMT, ts).explicitGet()
      ssTx1    = SetScriptTransaction.selfSigned(1.toByte, dApp1, Some(dApp1Script(dApp2.toAddress)), fee, ts).explicitGet()
      ssTx2    = SetScriptTransaction.selfSigned(1.toByte, dApp2, Some(dApp2Script), fee, ts).explicitGet()
      invokeTx = InvokeScriptTransaction.selfSigned(TxVersion.V3, invoker, dApp1.toAddress, None, Nil, fee, Waves, ts).explicitGet()
    } yield (Seq(gTx1, gTx2, gTx3, ssTx1, ssTx2), invokeTx)

  property("temprorary negative balance of sync call doesn't produce error before activation RideV6") {
    val (preparingTxs, invoke) = scenario.sample.get
    withDomain(RideV5) { d =>
      d.appendBlock(preparingTxs: _*)
      d.appendBlock(invoke)
      d.blockchain.transactionInfo(invoke.id.value()).get._3 shouldBe true
    }
  }

  property("temprorary negative balance of sync call produce error after activation RideV6") {
    val (preparingTxs, invoke) = scenario.sample.get
    withDomain(RideV6) { d =>
      d.appendBlock(preparingTxs: _*)
      (the[RuntimeException] thrownBy d.appendBlock(invoke)).getMessage should include(
        s"Sync call leads to negative balance = -100 for address ${invoke.dAppAddressOrAlias}")
    }
  }
}

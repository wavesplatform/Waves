package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.{StdLibVersion, V5, V6}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.test.*
import com.wavesplatform.transaction.{GenesisTransaction, TxHelpers, TxVersion}

class SyncDAppMultiVersionTest extends PropSpec with WithDomain {
  import DomainPresets.*

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private def dApp1Script(version: StdLibVersion, dApp2: Address): Script =
    TestCompiler(version).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |    strict r = Address(base58'$dApp2').invoke("default", [], [])
         |    []
         | }
       """.stripMargin
    )

  private def dApp2Script(version: StdLibVersion): Script =
    TestCompiler(version).compileContract(
      s"""
         | @Callable(i)
         | func default() = []
       """.stripMargin
    )

  private def scenario(version1: StdLibVersion, version2: StdLibVersion) =
    for {
      invoker <- accountGen
      dApp1   <- accountGen
      dApp2   <- accountGen
      fee     <- ciFee()
      gTx1     = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx2     = GenesisTransaction.create(dApp1.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx3     = GenesisTransaction.create(dApp2.toAddress, ENOUGH_AMT, ts).explicitGet()
      ssTx1    = TxHelpers.setScript(dApp1, dApp1Script(version1, dApp2.toAddress), fee, 1.toByte)
      ssTx2    = TxHelpers.setScript(dApp2, dApp2Script(version2), fee, 1.toByte)
      invokeTx = TxHelpers.invoke(dApp1.toAddress, invoker = invoker, fee = fee, version = TxVersion.V3, timestamp = ts)
    } yield (Seq(gTx1, gTx2, gTx3, ssTx1, ssTx2), invokeTx)

  property("sync call can be performed between V5 and V6 dApps") {
    Seq((V5, V6), (V6, V5))
      .foreach { case (version1, version2) =>
        val (preparingTxs, invoke) = scenario(version1, version2).sample.get
        withDomain(RideV6) { d =>
          d.appendBlock(preparingTxs*)
          d.appendBlock(invoke)
          d.blockchain.transactionSucceeded(invoke.txId) shouldBe true
        }
      }
  }
}

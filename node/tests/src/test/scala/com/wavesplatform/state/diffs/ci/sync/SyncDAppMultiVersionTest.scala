package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.{StdLibVersion, V5, V6}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.test._
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.utils.Signed

class SyncDAppMultiVersionTest extends PropSpec with WithDomain {
  import DomainPresets._

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
      ssTx1    = SetScriptTransaction.selfSigned(1.toByte, dApp1, Some(dApp1Script(version1, dApp2.toAddress)), fee, ts).explicitGet()
      ssTx2    = SetScriptTransaction.selfSigned(1.toByte, dApp2, Some(dApp2Script(version2)), fee, ts).explicitGet()
      invokeTx = Signed.invokeScript(TxVersion.V3, invoker, dApp1.toAddress, None, Nil, fee, Waves, ts)
    } yield (Seq(gTx1, gTx2, gTx3, ssTx1, ssTx2), invokeTx)

  property("sync call can be performed between V5 and V6 dApps") {
    Seq((V5, V6), (V6, V5))
      .foreach {
        case (version1, version2) =>
          val (preparingTxs, invoke) = scenario(version1, version2).sample.get
          withDomain(RideV6) { d =>
            d.appendBlock(preparingTxs*)
            d.appendBlock(invoke)
            d.blockchain.transactionSucceeded(invoke.txId) shouldBe true
          }
      }
  }
}

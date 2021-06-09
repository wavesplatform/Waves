package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V4, V5}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import com.wavesplatform.{NoShrink, TestTime, TransactionGen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{EitherValues, Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SyncDAppForbidOldVersionsTest
  extends PropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with TransactionGen
  with NoShrink
  with Inside
  with WithState
  with DBCacheSettings
  with MockFactory
  with WithDomain
  with EitherValues {
  import DomainPresets._

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private def proxyDAppScript(callingDApp: Address): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(inv)
         | func default() = {
         |    strict r = Address(base58'$callingDApp').invoke("default", nil, [])
         |    []
         | }
       """.stripMargin
    )

  private def callingDAppScript(version: StdLibVersion): Script = {
    val data =
      if (version == V4)
        " [] "
      else
        " WriteSet([]) "

    TestCompiler(version).compileContract(
      s"""
         | @Callable(inv)
         | func default() = $data
       """.stripMargin
    )
  }

  private def scenario(version: StdLibVersion) =
    for {
      invoker     <- accountGen
      callingDApp <- accountGen
      proxyDApp   <- accountGen
      fee         <- ciFee()
      gTx1     = GenesisTransaction.create(callingDApp.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx2     = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx3     = GenesisTransaction.create(proxyDApp.toAddress, ENOUGH_AMT, ts).explicitGet()
      ssTx     = SetScriptTransaction.selfSigned(1.toByte, callingDApp, Some(callingDAppScript(version)), fee, ts).explicitGet()
      ssTx2    = SetScriptTransaction.selfSigned(1.toByte, proxyDApp, Some(proxyDAppScript(callingDApp.toAddress)), fee, ts).explicitGet()
      invokeTx = InvokeScriptTransaction.selfSigned(TxVersion.V3, invoker, proxyDApp.toAddress, None, Nil, fee, Waves, ts).explicitGet()
    } yield (Seq(gTx1, gTx2, gTx3, ssTx, ssTx2), invokeTx, proxyDApp.toAddress, callingDApp.toAddress)

  property("sync call is forbidden for V3 and V4 DApps") {
    Seq(V3, V4)
      .foreach { callingDAppVersion =>
        val (preparingTxs, invoke, proxyDApp, callingDApp) = scenario(callingDAppVersion).sample.get
        withDomain(RideV5) { d =>
          d.appendBlock(preparingTxs: _*)
          (the[RuntimeException] thrownBy d.appendBlock(invoke)).getMessage should include(
            s"DApp $proxyDApp invoked DApp $callingDApp that uses RIDE $callingDAppVersion, " +
              s"but dApp-to-dApp invocation requires version 5 or higher"
          )
        }
      }
  }
}

package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V4, V5}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.{ENOUGH_AMT, ci}
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.test._
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.utils.Signed
import org.scalamock.scalatest.MockFactory
import org.scalatest.{EitherValues, Inside}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SyncDAppForbidOldVersionsTest
    extends PropSpec
    with ScalaCheckPropertyChecks
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

  private def scenario(version: StdLibVersion, invokeExpression: Boolean) =
    for {
      invoker     <- accountGen
      callingDApp <- accountGen
      proxyDApp   <- accountGen
      fee         <- ciFee()
      gTx1               = GenesisTransaction.create(callingDApp.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx2               = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx3               = GenesisTransaction.create(proxyDApp.toAddress, ENOUGH_AMT, ts).explicitGet()
      ssTx               = SetScriptTransaction.selfSigned(1.toByte, callingDApp, Some(callingDAppScript(version)), fee, ts).explicitGet()
      ssTx2              = SetScriptTransaction.selfSigned(1.toByte, proxyDApp, Some(proxyDAppScript(callingDApp.toAddress)), fee, ts).explicitGet()
      invokeScriptTx     = InvokeScriptTransaction.selfSigned(TxVersion.V3, invoker, proxyDApp.toAddress, None, Nil, fee, Waves, ts).explicitGet()
      invokeExpressionTx = ci.toInvokeExpression(ssTx2, invoker)
      invokeTx           = if (invokeExpression) invokeExpressionTx else invokeScriptTx
    } yield (Seq(gTx1, gTx2, gTx3, ssTx, ssTx2), invokeTx, proxyDApp.toAddress, callingDApp.toAddress)

  property("sync call is forbidden for V3 and V4 DApps") {
    for {
      callingDAppVersion <- Seq(V3, V4)
      invokeExpression   <- Seq(false, true)
    } {
      val (preparingTxs, invoke, proxyDApp, callingDApp) = scenario(callingDAppVersion, invokeExpression).sample.get
      val (settings, source, target)                     = if (invokeExpression) (RideV6, invoke.senderAddress, callingDApp) else (RideV5, proxyDApp, callingDApp)
      withDomain(settings) { d =>
        d.appendBlock(preparingTxs: _*)
        (the[RuntimeException] thrownBy d.appendBlock(invoke)).getMessage should include(
          s"DApp $source invoked DApp $target that uses RIDE $callingDAppVersion, " +
            s"but dApp-to-dApp invocation requires version 5 or higher"
        )
      }
    }
  }
}

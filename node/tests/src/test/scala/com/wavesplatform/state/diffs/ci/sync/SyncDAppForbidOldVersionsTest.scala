package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.account.Address
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V4, V5}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ci
import com.wavesplatform.test.*
import com.wavesplatform.transaction.{TxHelpers, TxVersion}
import org.scalatest.{EitherValues, Inside}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SyncDAppForbidOldVersionsTest
  extends PropSpec
    with ScalaCheckPropertyChecks
    with Inside
    with WithState
    with DBCacheSettings
    with WithDomain
    with EitherValues {

  import DomainPresets.*

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

  private def scenario(version: StdLibVersion, invokeExpression: Boolean) = {
    val invoker = TxHelpers.signer(0)
    val callingDApp = TxHelpers.signer(1)
    val proxyDApp = TxHelpers.signer(2)

    val balances = AddrWithBalance.enoughBalances(invoker, callingDApp, proxyDApp)

    val ssTx1 = TxHelpers.setScript(callingDApp, callingDAppScript(version))
    val ssTx2 = TxHelpers.setScript(proxyDApp, proxyDAppScript(callingDApp.toAddress))

    val invoke = TxHelpers.invoke(proxyDApp.toAddress, func = None, invoker = invoker, version = TxVersion.V3)
    val invokeTx = if (invokeExpression) ci.toInvokeExpression(ssTx2, invoker) else invoke

    (balances, Seq(ssTx1, ssTx2), invokeTx, proxyDApp.toAddress, callingDApp.toAddress)
  }

  property("sync call is forbidden for V3 and V4 DApps") {
    for {
      callingDAppVersion <- Seq(V3, V4)
      invokeExpression   <- Seq(false, true)
    } {
      val (balances, preparingTxs, invoke, proxyDApp, callingDApp) = scenario(callingDAppVersion, invokeExpression)
      val (settings, source, target) =
        if (invokeExpression)
          (ContinuationTransaction, invoke.sender.toAddress, callingDApp)
        else
          (RideV5, proxyDApp, callingDApp)
      withDomain(settings, balances) { d =>
        d.appendBlock(preparingTxs*)
        d.appendBlockE(invoke) should produce(
          s"DApp $source invoked DApp $target that uses RIDE $callingDAppVersion, " +
            s"but dApp-to-dApp invocation requires version 5 or higher"
        )
      }
    }
  }
}

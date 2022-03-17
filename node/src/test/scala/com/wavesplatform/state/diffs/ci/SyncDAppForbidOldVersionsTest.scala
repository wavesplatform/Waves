package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V4, V5}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
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

  property("sync call is forbidden for V3 and V4 DApps") {
    Seq(V3, V4).foreach { callingDAppVersion =>
      val invoker = TxHelpers.signer(0)
      val callingDApp = TxHelpers.signer(1)
      val proxyDApp = TxHelpers.signer(2)

      val balances = AddrWithBalance.enoughBalances(invoker, callingDApp, proxyDApp)

      val preparingTxs = Seq(
        TxHelpers.setScript(callingDApp, callingDAppScript(callingDAppVersion)),
        TxHelpers.setScript(proxyDApp, proxyDAppScript(callingDApp.toAddress))
      )
      val invoke = TxHelpers.invoke(proxyDApp.toAddress, func = None, invoker = invoker)

      withDomain(RideV5, balances) { d =>
        d.appendBlock(preparingTxs*)
        d.appendBlockE(invoke) should produce(
          s"DApp ${proxyDApp.toAddress} invoked DApp ${callingDApp.toAddress} that uses RIDE $callingDAppVersion, " +
            s"but dApp-to-dApp invocation requires version 5 or higher"
        )
      }
    }
  }
}

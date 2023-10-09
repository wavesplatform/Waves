package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V7
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BOOLEAN
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.DomainPresets.{BlockRewardDistribution, TransactionStateSnapshot}
import com.wavesplatform.test.{PropSpec, produce}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers.*

class SyncInvokePaymentValidationOrderTest extends PropSpec with WithDomain {
  private val issueTx = issue()
  private val asset   = IssuedAsset(issueTx.id())
  private val dApp = TestCompiler(V7).compileContract(
    s"""
       | @Callable(i)
       | func f1(fail: Boolean) = {
       |   strict r = Address(base58'$defaultAddress').invoke("f2", [fail], [AttachedPayment(base58'$asset', 123)])
       |   []
       | }
       |
       | @Callable(i)
       | func f2(fail: Boolean) = {
       |   strict c = if (fail) then ${(1 to 10).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")} else 0
       |   []
       | }
     """.stripMargin
  )

  property("sync invoke payment should be processed before calling dApp if light node isn't activated") {
    withDomain(BlockRewardDistribution, AddrWithBalance.enoughBalances(defaultSigner, secondSigner)) { d =>
      d.appendBlock(setScript(defaultSigner, dApp), setScript(secondSigner, dApp), issueTx)
      d.appendAndAssertFailed(invoke(invoker = secondSigner, func = Some("f1"), args = Seq(CONST_BOOLEAN(true))), "negative asset balance")
      d.appendBlockE(invoke(invoker = secondSigner, func = Some("f1"), args = Seq(CONST_BOOLEAN(false)))) should produce("negative asset balance")
    }
  }

  property("sync invoke payment should be processed before calling dApp if light node is activated") {
    withDomain(TransactionStateSnapshot, AddrWithBalance.enoughBalances(defaultSigner, secondSigner)) { d =>
      d.appendBlock(setScript(defaultSigner, dApp), setScript(secondSigner, dApp), issueTx)
      d.appendBlockE(invoke(invoker = secondSigner, func = Some("f1"), args = Seq(CONST_BOOLEAN(true)))) should produce("negative asset balance")
      d.appendBlockE(invoke(invoker = secondSigner, func = Some("f1"), args = Seq(CONST_BOOLEAN(false)))) should produce("negative asset balance")
    }
  }
}

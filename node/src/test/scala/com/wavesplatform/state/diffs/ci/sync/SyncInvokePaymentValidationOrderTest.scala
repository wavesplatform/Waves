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
       | func f1(bigComplexity: Boolean, error: Boolean) = {
       |   strict r = Address(base58'$defaultAddress').invoke("f2", [bigComplexity, error], [AttachedPayment(base58'$asset', 123)])
       |   []
       | }
       |
       | @Callable(i)
       | func f2(bigComplexity: Boolean, error: Boolean) = {
       |   strict c = if (bigComplexity) then ${(1 to 6).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")} else 0
       |   strict e = if (error) then throw("custom error") else 0
       |   []
       | }
     """.stripMargin
  )

  property("sync invoke payment should be validated after calling dApp if light node isn't activated") {
    withDomain(BlockRewardDistribution, AddrWithBalance.enoughBalances(defaultSigner, secondSigner)) { d =>
      d.appendBlock(setScript(defaultSigner, dApp), setScript(secondSigner, dApp), issueTx)
      d.appendAndAssertFailed(
        invoke(invoker = secondSigner, func = Some("f1"), args = Seq(CONST_BOOLEAN(true), CONST_BOOLEAN(false))),
        "negative asset balance"
      )
      d.appendBlockE(invoke(invoker = secondSigner, func = Some("f1"), args = Seq(CONST_BOOLEAN(false), CONST_BOOLEAN(false)))) should produce(
        "negative asset balance"
      )
    }
  }

  property("sync invoke payment should be validated before calling dApp if light node is activated") {
    withDomain(TransactionStateSnapshot, AddrWithBalance.enoughBalances(defaultSigner, secondSigner)) { d =>
      d.appendBlock(setScript(defaultSigner, dApp), setScript(secondSigner, dApp), issueTx)
      d.appendBlockE(invoke(invoker = secondSigner, func = Some("f1"), args = Seq(CONST_BOOLEAN(true), CONST_BOOLEAN(false)))) should produce(
        "negative asset balance"
      )
      d.appendBlockE(invoke(invoker = secondSigner, func = Some("f1"), args = Seq(CONST_BOOLEAN(false), CONST_BOOLEAN(false)))) should produce(
        "negative asset balance"
      )
    }
  }

  property("sync invoke should be correctly rejected and failed on enough balance and RIDE error if light node is activated") {
    withDomain(TransactionStateSnapshot, AddrWithBalance.enoughBalances(defaultSigner, secondSigner)) { d =>
      d.appendBlock(setScript(defaultSigner, dApp), setScript(secondSigner, dApp), issueTx)
      d.appendBlock(transfer(asset = asset))
      d.appendBlockE(invoke(invoker = secondSigner, func = Some("f1"), args = Seq(CONST_BOOLEAN(false), CONST_BOOLEAN(true)))) should produce(
        "custom error"
      )
      d.appendAndAssertFailed(
        invoke(invoker = secondSigner, func = Some("f1"), args = Seq(CONST_BOOLEAN(true), CONST_BOOLEAN(true))),
        "custom error"
      )
    }
  }
}

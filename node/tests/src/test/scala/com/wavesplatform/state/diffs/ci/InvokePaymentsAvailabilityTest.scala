package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.db.{DBCacheSettings, WithDomain}
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import org.scalatest.{EitherValues, Inside}

class InvokePaymentsAvailabilityTest extends PropSpec with Inside with DBCacheSettings with WithDomain with EitherValues {
  import DomainPresets.*

  private def proxyDAppScript(callingDApp: Address): Script =
    TestCompiler(V5).compileContract(
      s"""
         | {-# STDLIB_VERSION 5       #-}
         | {-# CONTENT_TYPE   DAPP    #-}
         | {-# SCRIPT_TYPE    ACCOUNT #-}
         |
         | let dApp2 = Address(base58'$callingDApp')
         |
         | @Callable(inv)
         | func default() = {
         |    let pmt = inv.payments[0]
         |    strict invokeV4 = dApp2.invoke("default", nil, [AttachedPayment(pmt.assetId, pmt.amount)])
         |    [
         |       IntegerEntry("balance_self", this.assetBalance(pmt.assetId.value())),
         |       IntegerEntry("balance_calling_dApp", dApp2.assetBalance(pmt.assetId.value()))
         |    ]
         | }
       """.stripMargin
    )

  private val callingDAppScript: Script = {
    TestCompiler(V5).compileContract(
      s"""
         | {-# CONTENT_TYPE   DAPP          #-}
         | {-# SCRIPT_TYPE    ACCOUNT       #-}
         |
         | @Callable(inv)
         | func default() = {
         |   let pmtAssetId = inv.payments[0].assetId.value()
         |   [
         |     IntegerEntry("balance_self", this.assetBalance(pmtAssetId)),
         |     IntegerEntry("balance_caller", inv.caller.assetBalance(pmtAssetId))
         |   ]
         | }
       """.stripMargin
    )
  }

  private val paymentAmount = 12345

  private def scenario(syncCall: Boolean) = {
    val invoker     = TxHelpers.signer(0)
    val callingDApp = TxHelpers.signer(1)
    val proxyDApp   = TxHelpers.signer(2)
    val balances    = AddrWithBalance.enoughBalances(invoker, callingDApp, proxyDApp)
    val issue       = TxHelpers.issue(invoker, ENOUGH_AMT)
    val ssTx        = TxHelpers.setScript(callingDApp, callingDAppScript)
    val ssTx2       = TxHelpers.setScript(proxyDApp, proxyDAppScript(callingDApp.toAddress))
    val asset       = IssuedAsset(issue.id.value())
    val payments    = Seq(Payment(paymentAmount, asset))
    val dApp        = if (syncCall) proxyDApp.toAddress else callingDApp.toAddress
    val invokeTx    = TxHelpers.invoke(dApp, payments = payments, invoker = invoker)
    (balances, Seq(ssTx, ssTx2, issue), invokeTx, callingDApp.toAddress, proxyDApp.toAddress, asset)
  }

  property("payments availability in usual call") {
    val (balances, preparingTxs, invoke, callingDApp, _, asset) = scenario(syncCall = false)
    withDomain(RideV5, balances) { d =>
      d.appendBlock(preparingTxs*)
      d.appendBlock(invoke)

      d.blockchain.transactionSucceeded(invoke.id.value()) shouldBe true
      d.blockchain.balance(invoke.senderAddress, asset) shouldBe ENOUGH_AMT - paymentAmount

      val expectingCallingDAppBalance = paymentAmount
      d.blockchain.balance(callingDApp, asset) shouldBe expectingCallingDAppBalance

      val expectingCallingDAppBalanceInsideCallingDApp = paymentAmount
      val expectingInvokerBalanceInsideCallingDApp     = ENOUGH_AMT - expectingCallingDAppBalanceInsideCallingDApp
      d.blockchain.accountData(callingDApp, "balance_self").get.value shouldBe expectingCallingDAppBalanceInsideCallingDApp
      d.blockchain.accountData(callingDApp, "balance_caller").get.value shouldBe expectingInvokerBalanceInsideCallingDApp
    }
  }

  property("payments availability in sync call") {
    val (balances, preparingTxs, invoke, callingDApp, proxyDApp, asset) = scenario(syncCall = true)
    withDomain(RideV5, balances) { d =>
      d.appendBlock(preparingTxs*)
      d.appendBlock(invoke)

      d.blockchain.transactionSucceeded(invoke.id.value()) shouldBe true
      d.blockchain.balance(invoke.senderAddress, asset) shouldBe ENOUGH_AMT - paymentAmount

      val expectingProxyDAppBalance = 0
      List[Any](
        d.blockchain.balance(proxyDApp, asset),
        d.blockchain.accountData(proxyDApp, "balance_self").get.value
      ).foreach(_ shouldBe expectingProxyDAppBalance)

      val expectingCallingDAppBalance = paymentAmount
      List[Any](
        d.blockchain.balance(callingDApp, asset),
        d.blockchain.accountData(proxyDApp, "balance_calling_dApp").get.value
      ).foreach(_ shouldBe expectingCallingDAppBalance)

      val expectingCallingDAppBalanceInsideCallingDApp = paymentAmount
      val expectingProxyDAppBalanceInsideCallingDApp   = 0
      d.blockchain.accountData(callingDApp, "balance_self").get.value shouldBe expectingCallingDAppBalanceInsideCallingDApp
      d.blockchain.accountData(callingDApp, "balance_caller").get.value shouldBe expectingProxyDAppBalanceInsideCallingDApp
    }
  }
}

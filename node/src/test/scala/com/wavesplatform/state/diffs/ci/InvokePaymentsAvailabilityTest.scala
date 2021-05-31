package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import com.wavesplatform.{NoShrink, TestTime, TransactionGen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{EitherValues, Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class InvokePaymentsAvailabilityTest
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

  private def scenario(syncCall: Boolean) =
    for {
      invoker     <- accountGen
      callingDApp <- accountGen
      proxyDApp   <- accountGen
      fee         <- ciFee()
      gTx1     = GenesisTransaction.create(callingDApp.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx2     = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx3     = GenesisTransaction.create(proxyDApp.toAddress, ENOUGH_AMT, ts).explicitGet()
      issue    = IssueTransaction.selfSigned(2.toByte, invoker, "name", "description", ENOUGH_AMT, 1, true, None, fee, ts).explicitGet()
      ssTx     = SetScriptTransaction.selfSigned(1.toByte, callingDApp, Some(callingDAppScript), fee, ts).explicitGet()
      ssTx2    = SetScriptTransaction.selfSigned(1.toByte, proxyDApp, Some(proxyDAppScript(callingDApp.toAddress)), fee, ts).explicitGet()
      asset    = IssuedAsset(issue.id.value())
      payments = Seq(Payment(paymentAmount, asset))
      dApp     = if (syncCall) proxyDApp.toAddress else callingDApp.toAddress
      invokeTx = InvokeScriptTransaction.selfSigned(TxVersion.V3, invoker, dApp, None, payments, fee, Waves, ts).explicitGet()
    } yield (Seq(gTx1, gTx2, gTx3, ssTx, ssTx2, issue), invokeTx, callingDApp.toAddress, proxyDApp.toAddress, asset)

  property("payments availability in usual call") {
    val (preparingTxs, invoke, callingDApp, _, asset) = scenario(syncCall = false).sample.get
    withDomain(RideV5) { d =>
      d.appendBlock(preparingTxs: _*)
      d.appendBlock(invoke)

      d.blockchain.transactionInfo(invoke.id.value()).get._3 shouldBe true
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
    val (preparingTxs, invoke, callingDApp, proxyDApp, asset) = scenario(syncCall = true).sample.get
    withDomain(RideV5) { d =>
      d.appendBlock(preparingTxs: _*)
      d.appendBlock(invoke)

      d.blockchain.transactionInfo(invoke.id.value()).get._3 shouldBe true
      d.blockchain.balance(invoke.senderAddress, asset) shouldBe ENOUGH_AMT - paymentAmount

      val expectingProxyDAppBalance = 0
      List(
        d.blockchain.balance(proxyDApp, asset),
        d.blockchain.accountData(proxyDApp, "balance_self").get.value
      ).foreach(_ shouldBe expectingProxyDAppBalance)

      val expectingCallingDAppBalance = paymentAmount
      List(
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

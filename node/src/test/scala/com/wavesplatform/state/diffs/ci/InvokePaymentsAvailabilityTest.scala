package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V5}
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

  private def syncDApp(dApp: Address): Script =
    TestCompiler(V5).compileContract(
      s"""
         | {-# STDLIB_VERSION 5       #-}
         | {-# CONTENT_TYPE   DAPP    #-}
         | {-# SCRIPT_TYPE    ACCOUNT #-}
         |
         | let dApp2 = Address(base58'$dApp')
         |
         | @Callable(inv)
         | func default() = {
         |    let pmt = inv.payments[0]
         |    strict invokeV4 = dApp2.invoke("stake", nil, [AttachedPayment(pmt.assetId, pmt.amount)])
         |    [
         |       IntegerEntry("balance_self", this.assetBalance(pmt.assetId.value())),
         |       IntegerEntry("balance_calling_dApp", dApp2.assetBalance(pmt.assetId.value()))
         |    ]
         | }
       """.stripMargin
    )

  private def dApp(version: StdLibVersion): Script = {
    val data =
      if (version > V3)
        s"""
           |let pmtAssetId = inv.payments[0].assetId.value()
           |[
           |  IntegerEntry("balance_self", this.assetBalance(pmtAssetId)),
           |  IntegerEntry("balance_caller", inv.caller.assetBalance(pmtAssetId))
           |]
         """.stripMargin
      else
        s"""
           |let pmtAssetId = inv.payment.value().assetId.value()
           |WriteSet([
           |  DataEntry("balance_self", this.assetBalance(pmtAssetId)),
           |  DataEntry("balance_caller", inv.caller.assetBalance(pmtAssetId))
           |])
         """.stripMargin

    TestCompiler(version).compileContract(
      s"""
         | {-# STDLIB_VERSION $version  #-}
         | {-# CONTENT_TYPE   DAPP      #-}
         | {-# SCRIPT_TYPE    ACCOUNT   #-}
         |
         | @Callable(inv)
         | func stake() = {
         |   $data
         | }
       """.stripMargin
    )
  }

  private val amount = 12345

  private def scenario(version: StdLibVersion) =
    for {
      invoker     <- accountGen
      callingDApp <- accountGen
      proxyDApp   <- accountGen
      fee         <- ciFee()
      gTx1     = GenesisTransaction.create(callingDApp.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx2     = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx3     = GenesisTransaction.create(proxyDApp.toAddress, ENOUGH_AMT, ts).explicitGet()
      issue    = IssueTransaction.selfSigned(2.toByte, invoker, "name", "description", ENOUGH_AMT, 1, true, None, fee, ts).explicitGet()
      ssTx     = SetScriptTransaction.selfSigned(1.toByte, callingDApp, Some(dApp(version)), fee, ts).explicitGet()
      ssTx2    = SetScriptTransaction.selfSigned(1.toByte, proxyDApp, Some(syncDApp(callingDApp.toAddress)), fee, ts).explicitGet()
      asset    = IssuedAsset(issue.id.value())
      payments = Seq(Payment(amount, asset))
      invokeTx = InvokeScriptTransaction.selfSigned(TxVersion.V3, invoker, proxyDApp.toAddress, None, payments, fee, Waves, ts).explicitGet()
    } yield (Seq(gTx1, gTx2, gTx3, ssTx, ssTx2, issue), invokeTx, callingDApp.toAddress, proxyDApp.toAddress, asset)

  property("payments availability") {
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V3)
      .foreach { v =>
        val (preparingTxs, invoke, callingDApp, proxyDApp, asset) = scenario(v).sample.get
        withDomain(RideV5) { d =>
          d.appendBlock(preparingTxs: _*)
          d.appendBlock(invoke)

          d.blockchain.transactionInfo(invoke.id.value()).get._3 shouldBe true
          d.blockchain.balance(invoke.senderAddress, asset) shouldBe ENOUGH_AMT - amount

          val (proxyDAppBalance, callingDAppBalance) = if (v >= V5) (0, amount) else (amount, 0)
          List(
            d.blockchain.balance(proxyDApp, asset),
            d.blockchain.accountData(proxyDApp, "balance_self").get.value,
            d.blockchain.accountData(callingDApp, "balance_caller").get.value
          ).foreach(_ shouldBe proxyDAppBalance)
          List(
            d.blockchain.balance(callingDApp, asset),
            d.blockchain.accountData(proxyDApp, "balance_calling_dApp").get.value,
            d.blockchain.accountData(callingDApp, "balance_self").get.value
          ).foreach(_ shouldBe callingDAppBalance)
        }
      }
  }
}

package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{StdLibVersion, V4, V5}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, Transaction, TxVersion}
import com.wavesplatform.{NoShrink, TestTime, TransactionGen}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.{EitherValues, Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class InvokePaymentsLimitTest
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

  private def dApp(version: StdLibVersion, nestedInvoke: Option[(Address, Seq[Payment])]): Script = {
    val nested = nestedInvoke.fold("") {
      case (address, payments) =>
        val paymentsStr = payments.map(p => s"AttachedPayment(base58'${p.assetId}', ${p.amount})").mkString("[", ", ", "]")
        s""" strict r = invoke(Address(base58'$address'), "default", [], $paymentsStr) """
    }
    TestCompiler(version).compileContract(
      s"""
         | {-# STDLIB_VERSION ${version.id} #-}
         | {-# CONTENT_TYPE   DAPP          #-}
         | {-# SCRIPT_TYPE    ACCOUNT       #-}
         |
         | @Callable(i)
         | func default() = {
         |   $nested
         |   []
         | }
       """.stripMargin
    )
  }

  private def scenario(version: StdLibVersion, paymentsCount: Int, nested: Boolean): Gen[(Seq[Transaction], InvokeScriptTransaction)] =
    for {
      invoker <- accountGen
      dApp1   <- accountGen
      dApp2   <- accountGen
      fee     <- ciFee()
      gTx1 = GenesisTransaction.create(dApp1.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx2 = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx3 = GenesisTransaction.create(dApp2.toAddress, ENOUGH_AMT, ts).explicitGet()
      issues = (1 to paymentsCount).map(
        _ => IssueTransaction.selfSigned(2.toByte, if (nested) dApp1 else invoker, "name", "description", 100, 1, true, None, fee, ts).explicitGet()
      )
      (nestedInvoke, txPayments) = {
        val payments = issues.map(i => Payment(1, IssuedAsset(i.id.value())))
        if (nested) (Some((dApp2.toAddress, payments)), Nil) else (None, payments)
      }
      ssTx     = SetScriptTransaction.selfSigned(1.toByte, dApp1, Some(dApp(version, nestedInvoke)), fee, ts).explicitGet()
      ssTx2    = SetScriptTransaction.selfSigned(1.toByte, dApp2, Some(dApp(version, None)), fee, ts).explicitGet()
      invokeTx = InvokeScriptTransaction.selfSigned(TxVersion.V3, invoker, dApp1.toAddress, None, txPayments, fee, Waves, ts).explicitGet()
    } yield (Seq(gTx1, gTx2, gTx3, ssTx, ssTx2) ++ issues, invokeTx)

  private def assertLimit(version: StdLibVersion, count: Int, nested: Boolean) = {
    val (preparingTxs, invoke) = scenario(version, count, nested).sample.get
    withDomain(RideV5) { d =>
      d.appendBlock(preparingTxs: _*)
      d.appendBlock(invoke)
      d.blockchain.transactionInfo(invoke.id.value()).get._3 shouldBe true
    }
    val (preparingTxs2, invoke2) = scenario(version, count + 1, nested).sample.get
    withDomain(RideV5) { d =>
      d.appendBlock(preparingTxs2: _*)
      (the[RuntimeException] thrownBy d.appendBlock(invoke2)).getMessage should include(
        s"Script payment amount=${count + 1} should not exceed $count"
      )
    }
  }

  property("payments limit") {
    assertLimit(V4, 2, nested = false)
    DirectiveDictionary[StdLibVersion].all.filter(_ >= V5).foreach(assertLimit(_, 10, nested = false))
    DirectiveDictionary[StdLibVersion].all.filter(_ >= V5).foreach(assertLimit(_, 10, nested = true))
  }
}

package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{StdLibVersion, V4, V5}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.{Transaction, TxHelpers}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{EitherValues, Inside}

class InvokePaymentsLimitTest extends PropSpec with Inside with WithState with DBCacheSettings with MockFactory with WithDomain with EitherValues {
  import DomainPresets._

  private def dApp(version: StdLibVersion, nestedInvoke: Option[(Address, Seq[Payment])]): Script = {
    val nested = nestedInvoke.fold("") {
      case (address, payments) =>
        val paymentsStr = payments.map(p => s"AttachedPayment(base58'${p.assetId}', ${p.amount})").mkString("[", ", ", "]")
        s""" strict r = invoke(Address(base58'$address'), "default", [], $paymentsStr) """
    }
    TestCompiler(version).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   $nested
         |   []
         | }
       """.stripMargin
    )
  }

  private def scenario(version: StdLibVersion, paymentsCount: Int, nested: Boolean): (Seq[Transaction], InvokeScriptTransaction) = {
    val invoker = TxHelpers.signer(0)
    val dApp1   = TxHelpers.signer(1)
    val dApp2   = TxHelpers.signer(2)
    val gTx1    = TxHelpers.genesis(dApp1.toAddress, ENOUGH_AMT)
    val gTx2    = TxHelpers.genesis(invoker.toAddress, ENOUGH_AMT)
    val gTx3    = TxHelpers.genesis(dApp2.toAddress, ENOUGH_AMT)
    val issues  = (1 to paymentsCount).map(_ => TxHelpers.issue(if (nested) dApp1 else invoker, 100))
    val (nestedInvoke, txPayments) = {
      val payments = issues.map(i => Payment(1, IssuedAsset(i.id.value())))
      if (nested)
        (Some((dApp2.toAddress, payments)), Nil)
      else
        (None, payments)
    }
    val ssTx     = TxHelpers.setScript(dApp1, dApp(version, nestedInvoke))
    val ssTx2    = TxHelpers.setScript(dApp2, dApp(version, None))
    val invokeTx = TxHelpers.invoke(dApp1.toAddress, payments = txPayments)
    (Seq(gTx1, gTx2, gTx3, ssTx, ssTx2) ++ issues, invokeTx)
  }

  private def assertLimit(version: StdLibVersion, count: Int, nested: Boolean) = {
    val (preparingTxs, invoke) = scenario(version, count, nested)
    withDomain(RideV5) { d =>
      d.appendBlock(preparingTxs: _*)
      d.appendBlock(invoke)
      d.blockchain.transactionSucceeded(invoke.id.value()) shouldBe true
    }
    val (preparingTxs2, invoke2) = scenario(version, count + 1, nested)
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

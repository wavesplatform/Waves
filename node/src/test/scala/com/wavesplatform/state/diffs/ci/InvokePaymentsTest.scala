package com.wavesplatform.state.diffs.ci
import com.wavesplatform.TestValues.invokeFee
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.StdLibVersion.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.{PropSpec, produce}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxHelpers._
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment

class InvokePaymentsTest extends PropSpec with WithDomain {
  import DomainPresets._

  property("invoke allowed if Transfer Transaction is prohibited in payment asset") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val assetScript = TestCompiler(V5).compileExpression(
        """
          | match tx {
          |   case tx: TransferTransaction => false
          |   case _                       => true
          | }
        """.stripMargin
      )
      val dApp = TestCompiler(V5).compileContract(
        """
          | @Callable(i)
          | func default() = []
        """.stripMargin
      )
      val issueTx = issue(script = Some(assetScript))
      val asset   = IssuedAsset(issueTx.id())
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlock(issueTx)
      d.appendBlock(invoke(payments = Seq(Payment(1, asset))))
      d.liquidDiff.scriptResults.head._2.error shouldBe None
    }
  }

  property("invoke on insufficient balance is always rejected for asset payment and fails on big complexity for waves") {
    val invoker = signer(2)
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner) :+ AddrWithBalance(invoker.toAddress, invokeFee)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = []
           |
           | @Callable(i)
           | func complex() = {
           |   strict c = ${(1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}
           |   []
           | }
         """.stripMargin
      )
      val issueTx = issue()
      val asset   = IssuedAsset(issueTx.id())
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlock(issueTx)
      d.appendBlockE(invoke(invoker = invoker, func = Some("complex"), payments = Seq(Payment(1, asset)))) should produce(
        s"Transaction application leads to negative asset '$asset' balance"
      )
      val invokeTx = invoke(invoker = invoker, func = Some("complex"), payments = Seq(Payment(1, Waves)))
      d.appendBlock(invokeTx)
      d.liquidDiff.errorMessage(invokeTx.txId).get.text should include("negative waves balance")
      d.appendBlockE(invoke(invoker = invoker, payments = Seq(Payment(1, Waves)))) should produce(s"negative waves balance")
    }
  }
}

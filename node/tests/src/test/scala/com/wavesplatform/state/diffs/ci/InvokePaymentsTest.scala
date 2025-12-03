package com.wavesplatform.state.diffs.ci
import com.wavesplatform.TestValues.invokeFee
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.LeaseBalance
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.test.{PropSpec, produce}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TransactionType
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment

class InvokePaymentsTest extends PropSpec with WithDomain {
  import DomainPresets.*

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
      d.appendAndAssertSucceed(invoke(payments = Seq(Payment(1, asset))))
    }
  }

  property("invoke fails if Transfer Transaction is allowed but Invoke is prohibited in payment asset") {
    def test(invokeCheck: String) = {
      withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
        val assetScript = TestCompiler(V5).compileAsset(
          s"""
             | match tx {
             |   case tx: InvokeScriptTransaction => $invokeCheck
             |   case _                           => true
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
        d.appendAndAssertFailed(invoke(payments = Seq(Payment(1, asset))), "Transaction is not allowed by script of the asset")
      }
    }
    test("tx.payments[0].assetId != this.id")
    test("false")
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
      d.appendAndAssertFailed(
        invoke(invoker = invoker, func = Some("complex"), payments = Seq(Payment(1, Waves))),
        "negative waves balance"
      )
      d.appendBlockE(invoke(invoker = invoker, payments = Seq(Payment(1, Waves)))) should produce(s"negative waves balance")
    }
  }

  property("trying to attach lease IN balance to invoke payment") {
    val invoker = signer(2)
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner) :+ AddrWithBalance(invoker.toAddress, invokeFee)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = []
         """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlock(lease(recipient = invoker.toAddress, amount = 1))
      d.blockchain.leaseBalance(invoker.toAddress) shouldBe LeaseBalance(1, 0)
      d.appendBlockE(invoke(invoker = invoker, payments = Seq(Payment(1, Waves)))) should produce(s"negative waves balance")
    }
  }

  property("trying to attach lease OUT balance to invoke payment") {
    val invoker  = signer(2)
    val leaseFee = FeeConstants(TransactionType.Lease) * FeeUnit
    withDomain(
      RideV5.configure(_.copy(blockVersion3AfterHeight = 0)),
      AddrWithBalance.enoughBalances(secondSigner) :+ AddrWithBalance(invoker.toAddress, leaseFee + invokeFee + 1)
    ) { d =>
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = []
         """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlock(lease(sender = invoker, recipient = defaultAddress, amount = 1))
      d.blockchain.leaseBalance(invoker.toAddress) shouldBe LeaseBalance(0, 1)
      d.appendBlockE(invoke(invoker = invoker, payments = Seq(Payment(1, Waves)))) should produce("negative effective balance")
    }
  }
}

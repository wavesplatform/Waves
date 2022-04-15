package com.wavesplatform.state.diffs.ci
import com.wavesplatform.TestValues.invokeFee
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.StdLibVersion.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.LeaseBalance
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.test.{PropSpec, produce}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxHelpers._
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
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
    val invoker        = signer(2)
    val leaseFee       = FeeConstants(LeaseTransaction.typeId) * FeeUnit
    val leaseCancelFee = FeeConstants(LeaseCancelTransaction.typeId) * FeeUnit
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner) :+ AddrWithBalance(invoker.toAddress, leaseFee + invokeFee + 1)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = []
         """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp))

      val leaseTx = lease(sender = invoker, recipient = defaultAddress, amount = 1)
      d.appendBlock(leaseTx)
      d.blockchain.balance(invoker.toAddress) shouldBe invokeFee + 1
      d.blockchain.leaseBalance(invoker.toAddress) shouldBe LeaseBalance(0, 1)
      d.blockchain.leaseBalance(defaultAddress) shouldBe LeaseBalance(1, 0)

      val startDAppBalance = d.blockchain.balance(secondAddress)
      val invokeTx         = invoke(invoker = invoker, payments = Seq(Payment(1, Waves)))
      d.appendBlock(invokeTx)
      d.liquidDiff.errorMessage(invokeTx.txId) shouldBe None
      d.blockchain.balance(invoker.toAddress) shouldBe 0
      d.blockchain.leaseBalance(invoker.toAddress) shouldBe LeaseBalance(0, 1)
      d.blockchain.leaseBalance(defaultAddress) shouldBe LeaseBalance(1, 0)
      val resultDAppBalance = d.blockchain.balance(secondAddress)
      val dAppBalanceDiff   = resultDAppBalance - startDAppBalance
      dAppBalanceDiff shouldBe 1

      d.appendBlock(transfer(to = invoker.toAddress, amount = leaseCancelFee))
      d.appendBlock(leaseCancel(leaseTx.id(), sender = invoker))
      d.blockchain.balance(invoker.toAddress) shouldBe 0
      d.blockchain.leaseBalance(invoker.toAddress) shouldBe LeaseBalance(0, 0)
      d.blockchain.leaseBalance(defaultAddress) shouldBe LeaseBalance(0, 0)
      d.blockchain.balance(secondAddress) shouldBe resultDAppBalance
    }
  }
}
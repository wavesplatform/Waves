package com.wavesplatform.state.diffs.smart.eth

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.EthTxGenerator
import com.wavesplatform.transaction.EthereumTransaction.Invocation
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.utils.EthHelpers

class EthereumInvokePaymentTest extends PropSpec with WithDomain with EthHelpers {
  import DomainPresets.*

  property("payment script should be executed before dApp") {
    def sigVerify(c: Boolean) =
      s""" strict c = ${if (c) (1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ") else "true"} """

    def dApp(bigComplexity: Boolean) = TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   ${sigVerify(bigComplexity)}
         |   []
         | }
       """.stripMargin
    )
    val paymentScript = TestCompiler(V5).compileExpression("throw()")
    val issueTx       = issue(script = Some(paymentScript))
    val asset         = IssuedAsset(issueTx.id())
    def invoke        = EthTxGenerator.generateEthInvoke(defaultEthSigner, secondAddress, "default", Nil, Seq(Payment(1, asset)))
    withDomain(RideV6, AddrWithBalance.enoughBalances(secondSigner) :+ AddrWithBalance(defaultSigner.toEthWavesAddress)) { d =>
      d.appendBlock(issueTx)

      d.appendBlock(setScript(secondSigner, dApp(bigComplexity = false)))
      d.appendBlockE(invoke) should produce("Explicit script termination")

      d.appendBlock(setScript(secondSigner, dApp(bigComplexity = true)))
      d.appendAndAssertFailed(invoke, "Explicit script termination")
    }
  }

  property("forbid zero and negative payments") {
    for {
      amount  <- Seq(-1, 0)
      isAsset <- Seq(true, false)
    } {
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = []
         """.stripMargin
      )
      val issueTx  = issue()
      val token    = if (isAsset) IssuedAsset(issueTx.id()) else Waves
      val payments = Seq(Payment(amount, token))
      val settings = RideV6.configure(_.copy(ethInvokePaymentsCheckHeight = 4))
      val balances = AddrWithBalance.enoughBalances(secondSigner) :+ AddrWithBalance(defaultSigner.toEthWavesAddress)
      def invoke   = EthTxGenerator.generateEthInvoke(defaultEthSigner, secondAddress, "default", Nil, payments)
      withDomain(settings, balances) { d =>
        d.appendBlock(issueTx, transfer(asset = token), setScript(secondSigner, dApp))

        val invoke1 = invoke
        d.appendAndAssertSucceed(invoke1)
        invoke1.payload.asInstanceOf[Invocation].toInvokeScriptLike(invoke, d.blockchain) shouldBe a[Right[_, _]]

        val invoke2 = invoke
        d.appendBlockE(invoke2) should produce("NonPositiveAmount")
        d.appendBlock()
        invoke2.payload.asInstanceOf[Invocation].toInvokeScriptLike(invoke, d.blockchain) should produce("NonPositiveAmount")
      }
    }
  }
}

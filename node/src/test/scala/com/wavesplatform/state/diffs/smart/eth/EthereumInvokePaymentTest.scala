package com.wavesplatform.state.diffs.smart.eth

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.transaction.utils.EthTxGenerator
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
}

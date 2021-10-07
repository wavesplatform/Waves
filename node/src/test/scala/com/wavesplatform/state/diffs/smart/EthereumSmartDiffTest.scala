package com.wavesplatform.state.diffs.smart

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.Portfolio
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.state.diffs.smart.predef.checkEthTransfer
import com.wavesplatform.test.{PropSpec, TestTime}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.{ABIConverter, EthereumTransaction, GenesisTransaction}
import com.wavesplatform.utils.EthHelpers

class EthereumSmartDiffTest extends PropSpec with WithDomain with EthHelpers {
  import DomainPresets._

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private val transferAmount = 1234

  private def script(ethTransfer: EthereumTransaction, recipient: Address) = TestCompiler(V6).compileExpression(
    s"""
       | let t = transferTransactionById(base58'${ethTransfer.id()}').value()
       | ${checkEthTransfer(ethTransfer, transferAmount, Waves, recipient)}
     """.stripMargin
  )

  private val expectedArg = 1

  private def dAppScript() = TestCompiler(V6).compileContract(
    s"""
       | @Callable(i)
       | func default(a: Int) = [ BooleanEntry("check", a == $expectedArg) ]
     """.stripMargin
  )

  private val scenario =
    for {
      fee       <- ciFee()
      recipient <- accountGen
      transfer    = EthereumTransaction.Transfer(None, transferAmount, recipient.toAddress)
      ethTransfer = EthereumTransaction(transfer, TestEthUnderlying, TestEthSignature, 'T'.toByte)
      gTx1        = GenesisTransaction.create(ethTransfer.senderAddress(), ENOUGH_AMT, ts).explicitGet()
      gTx2        = GenesisTransaction.create(recipient.toAddress, ENOUGH_AMT, ts).explicitGet()
      verifier    = Some(script(ethTransfer, recipient.toAddress))
      setVerifier = () => SetScriptTransaction.selfSigned(1.toByte, recipient, verifier, fee, ts).explicitGet()
    } yield (Seq(gTx1, gTx2, setVerifier()), ethTransfer, setVerifier(), recipient.toAddress)

  private val invokeScenario =
    for {
      fee  <- ciFee()
      dApp <- accountGen
      script    = dAppScript()
      id        = ABIConverter(script).funcByMethodId.collectFirst { case (id, f) if f.name == "default" => id.drop(2) }.get
      data      = s"${id}000000000000000000000000000000000000000000000000000000000000000${expectedArg}00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000000"
      invoke    = EthereumTransaction.Invocation(dApp.toAddress, data)
      ethInvoke = EthereumTransaction(invoke, TestEthUnderlying, TestEthSignature, 'T'.toByte)
      gTx1      = GenesisTransaction.create(ethInvoke.senderAddress(), ENOUGH_AMT, ts).explicitGet()
      gTx2      = GenesisTransaction.create(dApp.toAddress, ENOUGH_AMT, ts).explicitGet()
      setDApp   = SetScriptTransaction.selfSigned(1.toByte, dApp, Some(script), fee, ts).explicitGet()
    } yield (Seq(gTx1, gTx2, setDApp), ethInvoke, dApp.toAddress)

  property("transferTransactionById") {
    val (preparingTxs, ethTransfer, checkTx, recipient) = scenario.sample.get
    withDomain(RideV6) { d =>
      d.appendBlock(preparingTxs: _*)
      d.appendBlock(ethTransfer)

      d.liquidDiff.portfolios(recipient) shouldBe Portfolio.waves(transferAmount)
      d.liquidDiff.portfolios(ethTransfer.senderAddress()) shouldBe Portfolio.waves(-ethTransfer.underlying.getGasPrice.longValue() - transferAmount)

      d.appendBlock()
      d.appendBlock(checkTx)
      d.liquidDiff.scriptsRun shouldBe 1
    }
  }

  property("invoke") {
    val (preparingTxs, ethInvoke, dApp) = invokeScenario.sample.get
    withDomain(RideV6) { d =>
      d.appendBlock(preparingTxs: _*)
      d.appendBlock(ethInvoke)
      d.liquidDiff.scriptsRun shouldBe 1
      d.liquidDiff.accountData(dApp).data("check").value shouldBe true
    }
  }
}

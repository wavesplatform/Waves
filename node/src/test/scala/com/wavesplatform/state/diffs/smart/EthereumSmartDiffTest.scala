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
import com.wavesplatform.transaction.{EthereumTransaction, GenesisTransaction}
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
}

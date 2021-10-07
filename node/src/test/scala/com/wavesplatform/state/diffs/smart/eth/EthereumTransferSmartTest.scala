package com.wavesplatform.state.diffs.smart.eth

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

class EthereumTransferSmartTest extends PropSpec with WithDomain with EthHelpers {
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

  property("transferTransactionById") {
    val fee         = ciFee().sample.get
    val recipient   = accountGen.sample.get
    val transfer    = EthereumTransaction.Transfer(None, transferAmount, recipient.toAddress)
    val ethTransfer = EthereumTransaction(transfer, TestEthUnderlying, TestEthSignature, 'T'.toByte)
    val gTx1        = GenesisTransaction.create(ethTransfer.senderAddress(), ENOUGH_AMT, ts).explicitGet()
    val gTx2        = GenesisTransaction.create(recipient.toAddress, ENOUGH_AMT, ts).explicitGet()
    val verifier    = Some(script(ethTransfer, recipient.toAddress))
    val setVerifier = () => SetScriptTransaction.selfSigned(1.toByte, recipient, verifier, fee, ts).explicitGet()

    withDomain(RideV6) { d =>
      d.appendBlock(gTx1, gTx2, setVerifier())
      d.appendBlock(ethTransfer)

      d.liquidDiff.portfolios(recipient.toAddress) shouldBe Portfolio.waves(transferAmount)
      d.liquidDiff.portfolios(ethTransfer.senderAddress()) shouldBe Portfolio.waves(-ethTransfer.underlying.getGasPrice.longValue() - transferAmount)

      d.appendBlock()
      d.appendBlock(setVerifier())
      d.liquidDiff.scriptsRun shouldBe 1
    }
  }
}

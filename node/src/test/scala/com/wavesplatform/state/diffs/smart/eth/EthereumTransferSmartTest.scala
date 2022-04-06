package com.wavesplatform.state.diffs.smart.eth

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V6}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.Portfolio
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.smart.predef.{assertProvenPart, provenPart}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{ERC20Address, EthereumTransaction, GenesisTransaction, TxHelpers}
import com.wavesplatform.utils.EthHelpers

class EthereumTransferSmartTest extends PropSpec with WithDomain with EthHelpers {
  import DomainPresets.*

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private val transferAmount = 1234

  private def accountScript(version: StdLibVersion, getTx: String, tx: EthereumTransaction, asset: Option[IssuedAsset], recipient: Address) =
    TestCompiler(version).compileExpression(
      s"""
         | let t = $getTx(base58'${tx.id()}').${if (version >= V3) "value" else "extract"}()
         | ${if (version >= V3) checkEthTransfer(tx, Some(asset.fold("unit")(id => s"base58'$id'")), recipient) else "t == t"}
       """.stripMargin
    )

  private def assetScript(version: StdLibVersion, tx: EthereumTransaction, recipient: Address) =
    TestCompiler(version).compileAsset(
      s"""
         | match tx {
         |   case t: TransferTransaction =>
         |    if (t.version == 0)
         |      then {
         |        ${checkEthTransfer(tx, if (version >= V3) Some("this.id") else None, recipient)}
         |      } else {
         |        t.amount == $ENOUGH_AMT
         |      }
         |
         |   case _ => true
         | }
       """.stripMargin
    )

  private def checkEthTransfer(tx: EthereumTransaction, asset: Option[String], recipient: Address): String =
    s"""
       | ${provenPart(tx, emptyBodyBytes = true, checkProofs = false)}
       | let amount     = t.amount == $transferAmount
       | let feeAssetId = t.feeAssetId == unit
       | ${asset.fold("")(a => s"let assetId = t.assetId == $a")}
       | let attachment = t.attachment == base58'${ByteStr.empty}'
       | let recipient = match (t.recipient) {
       |   case a: Address => a.bytes == base58'$recipient'
       |   case a: Alias   => throw("unexpected")
       | }
       | ${assertProvenPart("t", proofs = false)} && amount && feeAssetId && recipient && attachment
       | ${if (asset.isEmpty) "" else " && assetId"}
     """.stripMargin

  property("access to Ethereum transfer from RIDE script") {
    val recipient = RandomKeyPair()

    val issue = IssueTransaction.selfSigned(2.toByte, recipient, "Asset", "", ENOUGH_AMT, 8, true, None, 1.waves, ts).explicitGet()
    val asset = IssuedAsset(issue.id())

    for {
      version <- DirectiveDictionary[StdLibVersion].all
      token   <- Seq(None, Some(ERC20Address(asset.id.take(20))))
    } {
      val transfer    = EthereumTransaction.Transfer(token, transferAmount, recipient.toAddress)
      val ethTransfer = EthereumTransaction(transfer, TestEthRawTransaction, TestEthSignature, 'T'.toByte)
      val ethSender   = ethTransfer.senderAddress()
      val preTransfer =
        TransferTransaction.selfSigned(2.toByte, recipient, ethSender, asset, ENOUGH_AMT, Waves, 0.001.waves, ByteStr.empty, ts).explicitGet()

      val genesis1 = GenesisTransaction.create(ethSender, ENOUGH_AMT, ts).explicitGet()
      val genesis2 = GenesisTransaction.create(recipient.toAddress, ENOUGH_AMT, ts).explicitGet()

      val function    = if (version >= V3) "transferTransactionById" else "transactionById"
      val verifier    = Some(accountScript(version, function, ethTransfer, token.map(_ => asset), recipient.toAddress))
      val setVerifier = () => SetScriptTransaction.selfSigned(1.toByte, recipient, verifier, 0.01.waves, ts).explicitGet()

      withDomain(RideV6) { d =>
        d.appendBlock(genesis1, genesis2, issue, preTransfer, setVerifier())
        d.appendBlock(ethTransfer)

        val transferPortfolio = if (token.isEmpty) Portfolio.waves(transferAmount) else Portfolio.build(asset, transferAmount)
        d.liquidDiff.portfolios(recipient.toAddress) shouldBe transferPortfolio
        d.liquidDiff.portfolios(ethSender) shouldBe Portfolio.waves(-ethTransfer.underlying.getGasPrice.longValue()).minus(transferPortfolio)

        d.appendBlock()

        if (version >= V6) {
          d.appendBlock(setVerifier()) // just for account script execution
          d.liquidDiff.scriptsRun shouldBe 1
        } else if (version >= V3) {
          (the[Exception] thrownBy d.appendBlock(setVerifier())).getMessage should include(
            "value() called on unit value on function 'transferTransactionById' call"
          )
        } else
          (the[Exception] thrownBy d.appendBlock(setVerifier())).getMessage should include(
            s"t = Left(CommonError(extract() called on unit value))"
          )
      }
    }
  }

  property("transfer scripted asset via Ethereum transaction") {
    val recipient = RandomKeyPair()

    val dummyTransfer    = EthereumTransaction.Transfer(None, transferAmount, recipient.toAddress)
    val dummyEthTransfer = EthereumTransaction(dummyTransfer, TestEthRawTransaction, TestEthSignature, 'T'.toByte) // needed to pass into asset script
    val ethSender        = dummyEthTransfer.senderAddress()

    val genesis1 = TxHelpers.genesis(ethSender, ENOUGH_AMT)
    val genesis2 = TxHelpers.genesis(recipient.toAddress, ENOUGH_AMT)

    DirectiveDictionary[StdLibVersion].all
      .foreach { version =>
        val script      = assetScript(version, dummyEthTransfer, recipient.toAddress)
        val issue       = IssueTransaction.selfSigned(2.toByte, recipient, "Asset", "", ENOUGH_AMT, 8, true, Some(script), 1.waves, ts).explicitGet()
        val asset       = IssuedAsset(issue.id())
        val ethTransfer = dummyEthTransfer.copy(dummyTransfer.copy(Some(ERC20Address(asset.id.take(20)))))
        val preTransfer =
          TransferTransaction.selfSigned(2.toByte, recipient, ethSender, asset, ENOUGH_AMT, Waves, 0.005.waves, ByteStr.empty, ts).explicitGet()

        withDomain(RideV6) { d =>
          d.appendBlock(genesis1, genesis2, issue, preTransfer)
          d.appendBlock(ethTransfer)

          d.liquidDiff.errorMessage(ethTransfer.id()) shouldBe None
          d.liquidDiff.portfolios(recipient.toAddress) shouldBe Portfolio.build(asset, transferAmount)
          d.liquidDiff.portfolios(ethTransfer.senderAddress()) shouldBe Portfolio(
            -ethTransfer.underlying.getGasPrice.longValue(),
            assets = Map(asset -> -transferAmount)
          )

          d.liquidDiff.scriptsComplexity should be > 0L
        }
      }
  }
}

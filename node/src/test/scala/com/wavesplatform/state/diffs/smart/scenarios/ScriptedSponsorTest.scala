package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.{IssueTransaction, SponsorFeeTransaction}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{GenesisTransaction, Transaction, TxVersion}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.PropSpec
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ScriptedSponsorTest extends PropSpec with PropertyChecks with WithState with TransactionGen with NoShrink {

  import com.wavesplatform.state.diffs._

  val ENOUGH_FEE: Long = 100000000

  val fs = TestFunctionalitySettings.Enabled
    .copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.NG.id                              -> 0,
        BlockchainFeatures.MassTransfer.id                    -> 0,
        BlockchainFeatures.SmartAccounts.id                   -> 0,
        BlockchainFeatures.DataTransaction.id                 -> 0,
        BlockchainFeatures.BurnAnyTokens.id                   -> 0,
        BlockchainFeatures.FeeSponsorship.id                  -> 0,
        BlockchainFeatures.FairPoS.id                         -> 0,
        BlockchainFeatures.SmartAssets.id                     -> 0,
        BlockchainFeatures.SmartAccountTrading.id             -> 0,
        BlockchainFeatures.SmallerMinimalGeneratingBalance.id -> 0
      ),
      featureCheckBlocksPeriod = 1,
      blocksForFeatureActivation = 1
    )

  private val estimator = ScriptEstimatorV2

  property("sponsorship works when used by scripted accounts") {
    forAll(separateContractAndSponsor) {
      case (setupTxs, transfer) =>
        val setupBlocks   = setupTxs.map(TestBlock.create)
        val transferBlock = TestBlock.create(Seq(transfer))

        val IssuedAsset(assetId) = transfer.feeAssetId
        val contract             = transfer.sender

        val contractSpent: Long = ENOUGH_FEE + 1
        val sponsorSpent: Long  = ENOUGH_FEE * 3 - 1 + ENOUGH_FEE * FeeValidation.FeeUnit

        val sponsor = setupTxs.flatten.collectFirst { case t: SponsorFeeTransaction => t.sender }.get

        assertDiffAndState(setupBlocks :+ TestBlock.create(Nil), transferBlock, fs) { (diff, blck) =>
          blck.balance(contract, IssuedAsset(assetId)) shouldEqual ENOUGH_FEE * 2
          blck.balance(contract) shouldEqual ENOUGH_AMT - contractSpent

          blck.balance(sponsor, IssuedAsset(assetId)) shouldEqual Long.MaxValue - ENOUGH_FEE * 2
          blck.balance(sponsor) shouldEqual ENOUGH_AMT - sponsorSpent
        }
    }
  }

  property("sponsorship works when sponsored by scripted accounts") {
    forAll(scriptedSponsor) {
      case (setupTxs, transfer) =>
        val setupBlocks   = setupTxs.map(TestBlock.create)
        val transferBlock = TestBlock.create(Seq(transfer))

        val IssuedAsset(assetId) = transfer.feeAssetId
        val contract             = setupTxs.flatten.collectFirst { case t: SponsorFeeTransaction => t.sender }.get
        val recipient            = transfer.sender

        val contractSpent: Long  = ENOUGH_FEE * 4 + ENOUGH_FEE * FeeValidation.FeeUnit
        val recipientSpent: Long = 1

        assertDiffAndState(setupBlocks :+ TestBlock.create(Nil), transferBlock, fs) { (diff, blck) =>
          blck.balance(contract, IssuedAsset(assetId)) shouldEqual Long.MaxValue - ENOUGH_FEE * 2
          blck.balance(contract) shouldEqual ENOUGH_AMT - contractSpent

          blck.balance(recipient, IssuedAsset(assetId)) shouldEqual ENOUGH_FEE * 2
          blck.balance(recipient) shouldEqual ENOUGH_AMT - recipientSpent
        }
    }
  }

  val scriptedSponsor = {
    val timestamp = System.currentTimeMillis()
    for {
      contract  <- accountGen
      recipient <- accountGen
      gen1 = GenesisTransaction
        .create(contract, ENOUGH_AMT, timestamp)
        .explicitGet()
      gen2 = GenesisTransaction
        .create(recipient, ENOUGH_AMT, timestamp)
        .explicitGet()
      (script, _) = ScriptCompiler(s"false", isAssetScript = false, estimator).explicitGet()
      issueTx = IssueTransaction
        .selfSigned(
          TxVersion.V1,
          sender = contract,
          name = "Asset#1".getBytes("UTF-8"),
          description = "description".getBytes("UTF-8"),
          quantity = Long.MaxValue,
          decimals = 8,
          reissuable = false,
          script = None,
          fee = ENOUGH_FEE,
          timestamp = timestamp + 2
        )
        .explicitGet()
      sponsorTx = SponsorFeeTransaction
        .selfSigned(1.toByte, contract, IssuedAsset(issueTx.id()), Some(1), ENOUGH_FEE, timestamp + 4)
        .explicitGet()
      transferToRecipient = TransferTransaction
        .selfSigned(
          2.toByte,
          contract,
          recipient,
          IssuedAsset(issueTx.id()),
          ENOUGH_FEE * 3,
          Waves,
          ENOUGH_FEE,
          Array.emptyByteArray,
          System.currentTimeMillis() + 4
        )
        .explicitGet()
      setScript = SetScriptTransaction
        .selfSigned(1.toByte, contract, Some(script), ENOUGH_FEE, System.currentTimeMillis() + 6)
        .explicitGet()
      transferTx = TransferTransaction
        .selfSigned(
          2.toByte,
          recipient,
          accountGen.sample.get,
          Waves,
          1,
          IssuedAsset(issueTx.id()),
          ENOUGH_FEE,
          Array.emptyByteArray,
          System.currentTimeMillis() + 8
        )
        .explicitGet()
    } yield (Seq(Seq(gen1, gen2), Seq(issueTx, sponsorTx), Seq(transferToRecipient, setScript)), transferTx)
  }

  val separateContractAndSponsor: Gen[(Seq[Seq[Transaction]], TransferTransaction)] = {
    val timestamp = System.currentTimeMillis()
    for {
      contract <- accountGen
      sponsor  <- accountGen
      gen1 = GenesisTransaction
        .create(contract, ENOUGH_AMT, timestamp)
        .explicitGet()
      gen2 = GenesisTransaction
        .create(sponsor, ENOUGH_AMT, timestamp)
        .explicitGet()
      (script, _) = ScriptCompiler(s"true", isAssetScript = false, estimator).explicitGet()
      issueTx = IssueTransaction
        .selfSigned(
          TxVersion.V1,
          sender = sponsor,
          name = "Asset#1".getBytes("UTF-8"),
          description = "description".getBytes("UTF-8"),
          quantity = Long.MaxValue,
          decimals = 8,
          reissuable = false,
          script = None,
          fee = ENOUGH_FEE,
          timestamp = timestamp + 2
        )
        .explicitGet()
      sponsorTx = SponsorFeeTransaction
        .selfSigned(1.toByte, sponsor, IssuedAsset(issueTx.id()), Some(1), ENOUGH_FEE, timestamp + 4)
        .explicitGet()
      transferToContract = TransferTransaction
        .selfSigned(
          2.toByte,
          sponsor,
          contract,
          IssuedAsset(issueTx.id()),
          ENOUGH_FEE * 3,
          Waves,
          ENOUGH_FEE,
          Array.emptyByteArray,
          System.currentTimeMillis() + 4
        )
        .explicitGet()
      setScript = SetScriptTransaction
        .selfSigned(1.toByte, contract, Some(script), ENOUGH_FEE, System.currentTimeMillis() + 6)
        .explicitGet()
      transferTx = TransferTransaction
        .selfSigned(
          2.toByte,
          contract,
          sponsor,
          Waves,
          1,
          IssuedAsset(issueTx.id()),
          ENOUGH_FEE,
          Array.emptyByteArray,
          System.currentTimeMillis() + 8
        )
        .explicitGet()
    } yield (Seq(Seq(gen1, gen2), Seq(issueTx, sponsorTx), Seq(transferToContract, setScript)), transferTx)
  }
}

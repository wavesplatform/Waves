package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.{IssueTransaction, SponsorFeeTransaction}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import com.wavesplatform.utils._

class ScriptedSponsorTest extends PropSpec with WithState {

  import com.wavesplatform.state.diffs._

  val ENOUGH_FEE: Long  = 100000000
  val SPONSOR_FEE: Long = 100000

  private val fs = TestFunctionalitySettings.Enabled
    .copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.BlockV5.id                         -> 0,
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
      case (setupTxs, transfer, assetId) =>
        val setupBlocks   = setupTxs.map(TestBlock.create)
        val transferBlock = TestBlock.create(Seq(transfer))

        val contract            = transfer.sender
        val contractSpent: Long = ENOUGH_FEE + 1
        val sponsorSpent: Long  = ENOUGH_FEE * 2 + SPONSOR_FEE - 1 + ENOUGH_FEE * FeeValidation.FeeUnit

        val sponsor = setupTxs.flatten.collectFirst { case t: SponsorFeeTransaction => t.sender }.get

        assertDiffAndState(setupBlocks :+ TestBlock.create(Nil), transferBlock, fs) { (_, blck) =>
          blck.balance(contract.toAddress, IssuedAsset(assetId)) shouldEqual ENOUGH_FEE * 2
          blck.balance(contract.toAddress) shouldEqual ENOUGH_AMT - contractSpent

          blck.balance(sponsor.toAddress, IssuedAsset(assetId)) shouldEqual Long.MaxValue - ENOUGH_FEE * 2
          blck.balance(sponsor.toAddress) shouldEqual ENOUGH_AMT - sponsorSpent
        }
    }
  }

  property("sponsorship works when sponsored by scripted accounts") {
    forAll(scriptedSponsor) {
      case (setupTxs, transfer, assetId) =>
        val setupBlocks   = setupTxs.map(TestBlock.create)
        val transferBlock = TestBlock.create(Seq(transfer))

        val contract  = setupTxs.flatten.collectFirst { case t: SponsorFeeTransaction => t.sender }.get
        val recipient = transfer.sender

        val contractSpent: Long  = ENOUGH_FEE * 3 + SPONSOR_FEE + ENOUGH_FEE * FeeValidation.FeeUnit
        val recipientSpent: Long = 1

        assertDiffAndState(setupBlocks :+ TestBlock.create(Nil), transferBlock, fs) { (_, blck) =>
          blck.balance(contract.toAddress, IssuedAsset(assetId)) shouldEqual Long.MaxValue - ENOUGH_FEE * 2
          blck.balance(contract.toAddress) shouldEqual ENOUGH_AMT - contractSpent

          blck.balance(recipient.toAddress, IssuedAsset(assetId)) shouldEqual ENOUGH_FEE * 2
          blck.balance(recipient.toAddress) shouldEqual ENOUGH_AMT - recipientSpent
        }
    }
  }

  private val scriptedSponsor = {
    val timestamp = System.currentTimeMillis()
    for {
      contract  <- accountGen
      recipient <- accountGen
      gen1 = GenesisTransaction
        .create(contract.toAddress, ENOUGH_AMT, timestamp)
        .explicitGet()
      gen2 = GenesisTransaction
        .create(recipient.toAddress, ENOUGH_AMT, timestamp)
        .explicitGet()
      (script, _) = ScriptCompiler(s"{-# STDLIB_VERSION 2 #-}\n false", isAssetScript = false, estimator).explicitGet()
      issueTx = IssueTransaction(
        TxVersion.V1,
        contract.publicKey,
        "Asset#1".utf8Bytes,
        "description".utf8Bytes,
        Long.MaxValue,
        8.toByte,
        false,
        None,
        ENOUGH_FEE,
        timestamp + 2
      ).signWith(contract.privateKey)
      sponsorTx = SponsorFeeTransaction
        .selfSigned(1.toByte, contract, IssuedAsset(issueTx.id()), Some(1), SPONSOR_FEE, timestamp + 4)
        .explicitGet()
      transferToRecipient = TransferTransaction
        .selfSigned(
          2.toByte,
          contract,
          recipient.toAddress,
          IssuedAsset(issueTx.id()),
          ENOUGH_FEE * 3,
          Waves,
          ENOUGH_FEE,
          ByteStr.empty,
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
          accountGen.sample.get.toAddress,
          Waves,
          1,
          IssuedAsset(issueTx.id()),
          ENOUGH_FEE,
          ByteStr.empty,
          System.currentTimeMillis() + 8
        )
        .explicitGet()
    } yield (Seq(Seq(gen1, gen2), Seq(issueTx, sponsorTx), Seq(transferToRecipient, setScript)), transferTx, issueTx.id())
  }

  private val separateContractAndSponsor = {
    val timestamp = System.currentTimeMillis()
    for {
      contract <- accountGen
      sponsor  <- accountGen
      gen1 = GenesisTransaction
        .create(contract.toAddress, ENOUGH_AMT, timestamp)
        .explicitGet()
      gen2 = GenesisTransaction
        .create(sponsor.toAddress, ENOUGH_AMT, timestamp)
        .explicitGet()
      (script, _) = ScriptCompiler(s"{-# STDLIB_VERSION 2 #-}\n true", isAssetScript = false, estimator).explicitGet()
      issueTx = IssueTransaction(
        TxVersion.V1,
        sponsor.publicKey,
        "Asset#1".utf8Bytes,
        "description".utf8Bytes,
        Long.MaxValue,
        8.toByte,
        false,
        None,
        ENOUGH_FEE,
        timestamp + 2
      ).signWith(sponsor.privateKey)
      sponsorTx = SponsorFeeTransaction
        .selfSigned(1.toByte, sponsor, IssuedAsset(issueTx.id()), Some(1), SPONSOR_FEE, timestamp + 4)
        .explicitGet()
      transferToContract = TransferTransaction
        .selfSigned(
          2.toByte,
          sponsor,
          contract.toAddress,
          IssuedAsset(issueTx.id()),
          ENOUGH_FEE * 3,
          Waves,
          ENOUGH_FEE,
          ByteStr.empty,
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
          sponsor.toAddress,
          Waves,
          1,
          IssuedAsset(issueTx.id()),
          ENOUGH_FEE,
          ByteStr.empty,
          System.currentTimeMillis() + 8
        )
        .explicitGet()
    } yield (Seq(Seq(gen1, gen2), Seq(issueTx, sponsorTx), Seq(transferToContract, setScript)), transferTx, issueTx.id())
  }
}

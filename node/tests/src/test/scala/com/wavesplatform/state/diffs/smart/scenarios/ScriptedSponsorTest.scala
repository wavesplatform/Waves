package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.SponsorFeeTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{TxHelpers, TxVersion}

class ScriptedSponsorTest extends PropSpec with WithState {

  import com.wavesplatform.state.diffs.*

  val ENOUGH_FEE: Long  = 100000000
  val SPONSOR_FEE: Long = 100000

  private val fs = TestFunctionalitySettings.Enabled
    .copy(
      featureCheckBlocksPeriod = 1,
      blocksForFeatureActivation = 1,
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
      )
    )

  private val estimator = ScriptEstimatorV2

  property("sponsorship works when used by scripted accounts") {
    val (setupTxs, transfer, assetId) = separateContractAndSponsor
    val setupBlocks                   = setupTxs.map(TestBlock.create)
    val transferBlock                 = TestBlock.create(Seq(transfer))

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

  property("sponsorship works when sponsored by scripted accounts") {
    val (setupTxs, transfer, assetId) = scriptedSponsor
    val setupBlocks                   = setupTxs.map(TestBlock.create)
    val transferBlock                 = TestBlock.create(Seq(transfer))

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

  private def scriptedSponsor = {
    val contract  = TxHelpers.signer(1)
    val recipient = TxHelpers.signer(2)
    val otherAcc  = TxHelpers.signer(3)

    val genesis             = Seq(contract, recipient).map(acc => TxHelpers.genesis(acc.toAddress))
    val (script, _)         = ScriptCompiler.compile(s"{-# STDLIB_VERSION 2 #-}\n true", estimator).explicitGet()
    val issue               = TxHelpers.issue(contract, Long.MaxValue, fee = ENOUGH_FEE, version = TxVersion.V1)
    val asset               = IssuedAsset(issue.id())
    val sponsorTx           = TxHelpers.sponsor(asset, Some(1), contract, fee = SPONSOR_FEE)
    val transferToRecipient = TxHelpers.transfer(contract, recipient.toAddress, ENOUGH_FEE * 3, asset, fee = ENOUGH_FEE)
    val setScript           = TxHelpers.setScript(contract, script, fee = ENOUGH_FEE)
    val transfer            = TxHelpers.transfer(recipient, otherAcc.toAddress, 1, feeAsset = asset, fee = ENOUGH_FEE)

    (Seq(genesis, Seq(issue, sponsorTx), Seq(transferToRecipient, setScript)), transfer, issue.id())
  }

  private def separateContractAndSponsor = {
    val contract = TxHelpers.signer(1)
    val sponsor  = TxHelpers.signer(2)

    val genesis            = Seq(contract, sponsor).map(acc => TxHelpers.genesis(acc.toAddress))
    val (script, _)        = ScriptCompiler.compile(s"{-# STDLIB_VERSION 2 #-}\n true", estimator).explicitGet()
    val issue              = TxHelpers.issue(sponsor, Long.MaxValue, fee = ENOUGH_FEE, version = TxVersion.V1)
    val asset              = IssuedAsset(issue.id())
    val sponsorTx          = TxHelpers.sponsor(asset, Some(1), sponsor, fee = SPONSOR_FEE)
    val transferToContract = TxHelpers.transfer(sponsor, contract.toAddress, ENOUGH_FEE * 3, asset, fee = ENOUGH_FEE)
    val setScript          = TxHelpers.setScript(contract, script, fee = ENOUGH_FEE)
    val transfer           = TxHelpers.transfer(contract, sponsor.toAddress, 1, feeAsset = asset, fee = ENOUGH_FEE)

    (Seq(genesis, Seq(issue, sponsorTx), Seq(transferToContract, setScript)), transfer, issue.id())
  }
}

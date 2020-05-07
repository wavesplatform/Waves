package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.settings.{BlockchainSettings, WavesSettings}
import com.wavesplatform.state._
import com.wavesplatform.state.diffs._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.{IssueTransaction, SponsorFeeTransaction}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{Asset, GenesisTransaction}
import org.scalacheck.Gen
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class BlockchainUpdaterSponsoredFeeBlockTest
    extends PropSpec
    with PropertyChecks
    with DomainScenarioDrivenPropertyCheck
    with Matchers
    with TransactionGen {

  private val amtTx = 100000

  type Setup =
    (GenesisTransaction, TransferTransaction, IssueTransaction, SponsorFeeTransaction, TransferTransaction, TransferTransaction, TransferTransaction)

  val sponsorPreconditions: Gen[Setup] = for {

    master                      <- accountGen
    ts                          <- timestampGen
    transferAssetWavesFee       <- smallFeeGen
    _                           <- accountGen
    alice                       <- accountGen
    bob                         <- accountGen
    (feeAsset, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(alice)
    wavesFee                    = Sponsorship.toWaves(sponsorTx.minSponsoredAssetFee.get, sponsorTx.minSponsoredAssetFee.get)
    genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
    masterToAlice: TransferTransaction = TransferTransaction
      .selfSigned(
        1.toByte,
        master,
        alice.toAddress,
        Waves,
        feeAsset.fee + sponsorTx.fee + transferAssetWavesFee + wavesFee,
        Waves,
        transferAssetWavesFee,
        None,
        ts + 1
      )
      .right
      .get
    aliceToBob: TransferTransaction = TransferTransaction
      .selfSigned(
        1.toByte,
        alice,
        bob.toAddress,
        Asset.fromCompatId(Some(feeAsset.id())),
        feeAsset.quantity / 2,
        Waves,
        transferAssetWavesFee,
        None,
        ts + 2
      )
      .right
      .get
    bobToMaster: TransferTransaction = TransferTransaction
      .selfSigned(
        1.toByte,
        bob,
        master.toAddress,
        Asset.fromCompatId(Some(feeAsset.id())),
        amtTx,
        Asset.fromCompatId(Some(feeAsset.id())),
        sponsorTx.minSponsoredAssetFee.get,
        None,
        ts + 3
      )
      .right
      .get
    bobToMaster2: TransferTransaction = TransferTransaction
      .selfSigned(
        1.toByte,
        bob,
        master.toAddress,
        Asset.fromCompatId(Some(feeAsset.id())),
        amtTx,
        Asset.fromCompatId(Some(feeAsset.id())),
        sponsorTx.minSponsoredAssetFee.get,
        None,
        ts + 4
      )
      .right
      .get
  } yield (genesis, masterToAlice, feeAsset, sponsorTx, aliceToBob, bobToMaster, bobToMaster2)

  val SponsoredFeeActivatedAt0BlockchainSettings: BlockchainSettings = DefaultBlockchainSettings.copy(
    functionalitySettings = DefaultBlockchainSettings.functionalitySettings
      .copy(
        featureCheckBlocksPeriod = 1,
        blocksForFeatureActivation = 1,
        preActivatedFeatures = Map(
          BlockchainFeatures.FeeSponsorship.id -> 0,
          BlockchainFeatures.NG.id -> 0,
          BlockchainFeatures.BlockV5.id -> 0
        )
      )
  )

  val SponsoredActivatedAt0WavesSettings: WavesSettings = settings.copy(blockchainSettings = SponsoredFeeActivatedAt0BlockchainSettings)

  property("not enough waves to sponsor sponsored tx") {
    scenario(sponsorPreconditions, SponsoredActivatedAt0WavesSettings) {
      case (domain, (genesis, masterToAlice, feeAsset, sponsor, aliceToBob, bobToMaster, bobToMaster2)) =>
        val (block0, microBlocks) = chainBaseAndMicro(randomSig, genesis, Seq(masterToAlice, feeAsset, sponsor).map(Seq(_)))
        val block1 =
          customBuildBlockOfTxs(microBlocks.last.totalResBlockSig, Seq.empty, KeyPair(Array.fill(KeyLength)(1: Byte)), 3: Byte, sponsor.timestamp + 1)
        val block2 = customBuildBlockOfTxs(block1.id(), Seq.empty, KeyPair(Array.fill(KeyLength)(1: Byte)), 3: Byte, sponsor.timestamp + 1)
        val block3 = buildBlockOfTxs(block2.id(), Seq(aliceToBob, bobToMaster))
        val block4 = buildBlockOfTxs(block3.id(), Seq(bobToMaster2))

        domain.blockchainUpdater.processBlock(block0).explicitGet()
        domain.blockchainUpdater.processMicroBlock(microBlocks(0)).explicitGet()
        domain.blockchainUpdater.processMicroBlock(microBlocks(1)).explicitGet()
        domain.blockchainUpdater.processMicroBlock(microBlocks(2)).explicitGet()
        domain.blockchainUpdater.processBlock(block1).explicitGet()
        domain.blockchainUpdater.processBlock(block2).explicitGet()
        domain.blockchainUpdater.processBlock(block3).explicitGet()
        domain.blockchainUpdater.processBlock(block4) should produce("negative waves balance" /*"unavailable funds"*/ )
    }
  }

  property("calculates valid total fee for microblocks") {
    scenario(sponsorPreconditions, SponsoredActivatedAt0WavesSettings) {
      case (domain, (genesis, masterToAlice, feeAsset, sponsor, aliceToBob, bobToMaster, _)) =>
        val (block0, microBlocks) = chainBaseAndMicro(randomSig, genesis, Seq(Seq(masterToAlice, feeAsset, sponsor), Seq(aliceToBob, bobToMaster)))

        val block0TotalFee = block0.transactionData
          .filter(_.assetFee._1 == Waves)
          .map(_.assetFee._2)
          .sum

        {
          domain.blockchainUpdater.processBlock(block0) shouldBe 'right
          domain.blockchainUpdater.bestLiquidDiffAndFees.map(_._3) should contain(block0TotalFee)
        }

        {
          domain.blockchainUpdater.processMicroBlock(microBlocks(0)) shouldBe 'right
          domain.blockchainUpdater.processMicroBlock(microBlocks(1)) shouldBe 'right

          val microBlocksWavesFee = microBlocks
            .flatMap(_.transactionData)
            .map(tx => Sponsorship.calcWavesFeeAmount(tx, ai => domain.blockchainUpdater.assetDescription(ai).map(_.sponsorship)))
            .sum

          domain.blockchainUpdater.bestLiquidDiffAndFees.map(_._3) should contain(block0TotalFee + microBlocksWavesFee)
        }
    }
  }

}

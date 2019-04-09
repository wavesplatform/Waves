package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto._
import com.wavesplatform.features.BlockchainFeatures
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
    (GenesisTransaction,
     TransferTransactionV1,
     IssueTransaction,
     SponsorFeeTransaction,
     TransferTransactionV1,
     TransferTransactionV1,
     TransferTransactionV1)

  val sponsorPreconditions: Gen[Setup] = for {

    master                      <- accountGen
    ts                          <- timestampGen
    transferAssetWavesFee       <- smallFeeGen
    sponsor                     <- accountGen
    alice                       <- accountGen
    bob                         <- accountGen
    (feeAsset, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(alice)
    wavesFee                    = Sponsorship.toWaves(sponsorTx.minSponsoredAssetFee.get, sponsorTx.minSponsoredAssetFee.get)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    masterToAlice: TransferTransactionV1 = TransferTransactionV1
      .selfSigned(Waves,
                  master,
                  alice,
                  feeAsset.fee + sponsorTx.fee + transferAssetWavesFee + wavesFee,
                  ts + 1,
                  Waves,
                  transferAssetWavesFee,
                  Array.emptyByteArray)
      .right
      .get
    aliceToBob: TransferTransactionV1 = TransferTransactionV1
      .selfSigned(
        Asset.fromCompatId(Some(feeAsset.id())),
        alice,
        bob,
        feeAsset.quantity / 2,
        ts + 2,
        Waves,
        transferAssetWavesFee,
        Array.emptyByteArray
      )
      .right
      .get
    bobToMaster: TransferTransactionV1 = TransferTransactionV1
      .selfSigned(
        Asset.fromCompatId(Some(feeAsset.id())),
        bob,
        master,
        amtTx,
        ts + 3,
        Asset.fromCompatId(Some(feeAsset.id())),
        sponsorTx.minSponsoredAssetFee.get,
        Array.emptyByteArray
      )
      .right
      .get
    bobToMaster2: TransferTransactionV1 = TransferTransactionV1
      .selfSigned(
        Asset.fromCompatId(Some(feeAsset.id())),
        bob,
        master,
        amtTx,
        ts + 4,
        Asset.fromCompatId(Some(feeAsset.id())),
        sponsorTx.minSponsoredAssetFee.get,
        Array.emptyByteArray
      )
      .right
      .get
  } yield (genesis, masterToAlice, feeAsset, sponsorTx, aliceToBob, bobToMaster, bobToMaster2)

  val SponsoredFeeActivatedAt0BlockchainSettings: BlockchainSettings = DefaultBlockchainSettings.copy(
    functionalitySettings = DefaultBlockchainSettings.functionalitySettings
      .copy(featureCheckBlocksPeriod = 1,
            blocksForFeatureActivation = 1,
            preActivatedFeatures = Map(BlockchainFeatures.FeeSponsorship.id -> 0, BlockchainFeatures.NG.id -> 0)))

  val SponsoredActivatedAt0WavesSettings: WavesSettings = settings.copy(blockchainSettings = SponsoredFeeActivatedAt0BlockchainSettings)

  property("not enough waves to sponsor sponsored tx") {
    scenario(sponsorPreconditions, SponsoredActivatedAt0WavesSettings) {
      case (domain, (genesis, masterToAlice, feeAsset, sponsor, aliceToBob, bobToMaster, bobToMaster2)) =>
        val (block0, microBlocks) = chainBaseAndMicro(randomSig, genesis, Seq(masterToAlice, feeAsset, sponsor).map(Seq(_)))
        val block1 = customBuildBlockOfTxs(microBlocks.last.totalResBlockSig,
                                           Seq.empty,
                                           KeyPair(Array.fill(KeyLength)(1: Byte)),
                                           3: Byte,
                                           sponsor.timestamp + 1)
        val block2 = customBuildBlockOfTxs(block1.uniqueId, Seq.empty, KeyPair(Array.fill(KeyLength)(1: Byte)), 3: Byte, sponsor.timestamp + 1)
        val block3 = buildBlockOfTxs(block2.uniqueId, Seq(aliceToBob, bobToMaster))
        val block4 = buildBlockOfTxs(block3.uniqueId, Seq(bobToMaster2))

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
      case (domain, (genesis, masterToAlice, feeAsset, sponsor, aliceToBob, bobToMaster, bobToMaster2)) =>
        val (block0, microBlocks) = chainBaseAndMicro(randomSig, genesis, Seq(Seq(masterToAlice, feeAsset, sponsor), Seq(aliceToBob, bobToMaster)))

        val block0TotalFee = block0.transactionData
          .filter(_.assetFee._1 == Waves)
          .map(_.assetFee._2)
          .sum

        {
          domain.blockchainUpdater.processBlock(block0) shouldBe 'right
          domain.blockchainUpdater.totalFee(domain.blockchainUpdater.height) should contain(block0TotalFee)
        }

        {
          domain.blockchainUpdater.processMicroBlock(microBlocks(0)) shouldBe 'right
          domain.blockchainUpdater.processMicroBlock(microBlocks(1)) shouldBe 'right

          val microBlocksWavesFee = microBlocks
            .flatMap(_.transactionData)
            .map(tx => Sponsorship.calcWavesFeeAmount(tx, ai => domain.blockchainUpdater.assetDescription(ai).map(_.sponsorship)))
            .sum

          domain.blockchainUpdater.totalFee(domain.blockchainUpdater.height) should contain(block0TotalFee + microBlocksWavesFee)
        }
    }
  }

}

package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.{BlockchainSettings, Constants, WavesSettings}
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.GenesisTransaction
import scorex.transaction.assets.{IssueTransaction, SponsorFeeTransaction, TransferTransaction}

class BlockchainUpdaterSponsoredFeeBlockTest
    extends PropSpec
    with PropertyChecks
    with DomainScenarioDrivenPropertyCheck
    with Matchers
    with TransactionGen {

  type Setup = (GenesisTransaction, IssueTransaction, SponsorFeeTransaction, TransferTransaction)

  val sponsorPreconditions: Gen[Setup] = for {

    master                      <- accountGen
    ts                          <- timestampGen
    fee                         <- smallFeeGen
    alice                       <- accountGen
    (feeAsset, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(master)
    genesis: GenesisTransaction = GenesisTransaction.create(master, feeAsset.fee + sponsorTx.fee, ts).right.get
    transferTx: TransferTransaction = TransferTransaction
      .create(Some(feeAsset.id()), master, alice, 1 * Constants.UnitsInWave, ts, Some(feeAsset.id()), fee, Array.emptyByteArray)
      .right
      .get
  } yield (genesis, feeAsset, sponsorTx, transferTx)

  val SponsoredFeeActivatedAt0BlockchainSettings: BlockchainSettings = DefaultBlockchainSettings.copy(
    functionalitySettings = DefaultBlockchainSettings.functionalitySettings.copy(
      preActivatedFeatures = Map(BlockchainFeatures.SponsoredFee.id -> 0, BlockchainFeatures.NG.id -> 0)))

  val SponsoredActivatedAt0WavesSettings: WavesSettings = settings.copy(blockchainSettings = SponsoredFeeActivatedAt0BlockchainSettings)

  property("not enough waves to sponsor") {
    scenario(sponsorPreconditions, SponsoredActivatedAt0WavesSettings) {
      case (domain, (genesis, feeAsset, sponsor, transfer)) =>
        val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, Seq(feeAsset, sponsor, transfer).map(Seq(_)))
        domain.blockchainUpdater.processBlock(block).explicitGet()
        domain.blockchainUpdater.processMicroBlock(microBlocks(0)).explicitGet()
        domain.blockchainUpdater.processMicroBlock(microBlocks(1)).explicitGet()
        domain.blockchainUpdater.processMicroBlock(microBlocks(2)) should produce("unavailable funds")
    }
  }

}

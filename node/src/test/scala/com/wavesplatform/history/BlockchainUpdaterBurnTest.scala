package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.settings.{BlockchainSettings, WavesSettings}
import com.wavesplatform.state.diffs.{ENOUGH_AMT, produce}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction}
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, GenesisTransaction, TxVersion}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class BlockchainUpdaterBurnTest extends PropSpec with PropertyChecks with DomainScenarioDrivenPropertyCheck with Matchers with TransactionGen {
  val Waves: Long = 100000000

  type Setup =
    (Long, GenesisTransaction, TransferTransaction, IssueTransaction, BurnTransaction, ReissueTransaction)

  val preconditions: Gen[Setup] = for {
    master                                                   <- accountGen
    ts                                                       <- timestampGen
    transferAssetWavesFee                                    <- smallFeeGen
    alice                                                    <- accountGen
    (_, assetName, description, quantity, decimals, _, _, _) <- issueParamGen
    genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
    masterToAlice: TransferTransaction = TransferTransaction
      .selfSigned(1.toByte, master, alice.toAddress, Asset.Waves, 3 * Waves, Asset.Waves, transferAssetWavesFee, None, ts + 1)
      .explicitGet()
    issue: IssueTransaction = IssueTransaction(TxVersion.V1, alice.publicKey, assetName, description, quantity, decimals, false, script = None, Waves, ts + 100)
      .signWith(alice.privateKey)
    burn: BurnTransaction = BurnTransaction.selfSigned(1.toByte, alice, IssuedAsset(issue.assetId), quantity / 2, Waves, ts + 200).explicitGet()
    reissue: ReissueTransaction = ReissueTransaction
      .selfSigned(1.toByte, alice, IssuedAsset(issue.assetId), burn.quantity, true, Waves, ts + 300)
      .explicitGet()
  } yield (ts, genesis, masterToAlice, issue, burn, reissue)

  val localBlockchainSettings: BlockchainSettings = DefaultBlockchainSettings.copy(
    functionalitySettings = DefaultBlockchainSettings.functionalitySettings
      .copy(
        featureCheckBlocksPeriod = 1,
        blocksForFeatureActivation = 1,
        preActivatedFeatures = Map(BlockchainFeatures.NG.id -> 0, BlockchainFeatures.DataTransaction.id -> 0)
      )
  )
  val localWavesSettings: WavesSettings = settings.copy(blockchainSettings = localBlockchainSettings)

  property("issue -> burn -> reissue in sequential blocks works correctly") {
    scenario(preconditions, localWavesSettings) {
      case (domain, (ts, genesis, masterToAlice, issue, burn, reissue)) =>
        val block0 = customBuildBlockOfTxs(randomSig, Seq(genesis), defaultSigner, 1.toByte, ts)
        val block1 = customBuildBlockOfTxs(block0.id(), Seq(masterToAlice), defaultSigner, TxVersion.V1, ts + 150)
        val block2 = customBuildBlockOfTxs(block1.id(), Seq(issue), defaultSigner, TxVersion.V1, ts + 250)
        val block3 = customBuildBlockOfTxs(block2.id(), Seq(burn), defaultSigner, TxVersion.V1, ts + 350)
        val block4 = customBuildBlockOfTxs(block3.id(), Seq(reissue), defaultSigner, TxVersion.V1, ts + 450)

        domain.appendBlock(block0)
        domain.appendBlock(block1)

        domain.appendBlock(block2)
        val assetDescription1 = domain.blockchainUpdater.assetDescription(IssuedAsset(issue.assetId)).get
        assetDescription1.reissuable should be(false)
        assetDescription1.totalVolume should be(issue.quantity)

        domain.appendBlock(block3)
        val assetDescription2 = domain.blockchainUpdater.assetDescription(IssuedAsset(issue.assetId)).get
        assetDescription2.reissuable should be(false)
        assetDescription2.totalVolume should be(issue.quantity - burn.quantity)

        domain.blockchainUpdater.processBlock(block4) should produce("Asset is not reissuable")
    }
  }

  property("issue -> burn -> reissue in micro blocks works correctly") {
    scenario(preconditions, localWavesSettings) {
      case (domain, (ts, genesis, masterToAlice, issue, burn, reissue)) =>
        val block0 = customBuildBlockOfTxs(randomSig, Seq(genesis), defaultSigner, TxVersion.V1, ts)
        val block1 = customBuildBlockOfTxs(block0.id(), Seq(masterToAlice), defaultSigner, TxVersion.V1, ts + 150)
        val block2 = customBuildBlockOfTxs(block1.id(), Seq(issue), defaultSigner, TxVersion.V1, ts + 250)
        val block3 = customBuildBlockOfTxs(block2.id(), Seq(burn, reissue), defaultSigner, TxVersion.V1, ts + 350)

        domain.appendBlock(block0)
        domain.appendBlock(block1)

        domain.appendBlock(block2)
        val assetDescription1 = domain.blockchainUpdater.assetDescription(IssuedAsset(issue.assetId)).get
        assetDescription1.reissuable should be(false)
        assetDescription1.totalVolume should be(issue.quantity)

        domain.blockchainUpdater.processBlock(block3) should produce("Asset is not reissuable")
    }
  }
}

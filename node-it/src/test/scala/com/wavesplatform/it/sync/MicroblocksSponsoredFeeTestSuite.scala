package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.{BaseFreeSpec, NodeConfigs}
import com.wavesplatform.state.{Height, Sponsorship}
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.utils.ScorexLogging

class MicroblocksSponsoredFeeTestSuite extends BaseFreeSpec with ScorexLogging {

  private lazy val sponsor      = nodes(1)
  private val Token             = 100L
  private val sponsorAssetTotal = 100000 * Token
  private val minSponsorFee     = Token
  private val SmallFee          = Token + Token / 2

  private def secondAddress = nodes(2).address

  private def txRequestsGen(n: Int, sponsorAssetId: String): Seq[String] =
    (1 to n).map(_ => sponsor.transfer(sponsor.keyPair, secondAddress, Token, fee = SmallFee, None, Some(sponsorAssetId)).id)

  "fee distribution with sponsorship" - {
    "check fee distribution" in {
      val sponsorAssetId = sponsor
        .issue(sponsor.keyPair, "SponsoredAsset", "Created by Sponsorship Suite", sponsorAssetTotal, reissuable = false, fee = issueFee)
        .id
      nodes.waitForHeightAriseAndTxPresent(sponsorAssetId)

      val transferTxToSecondAddress = sponsor.transfer(sponsor.keyPair, secondAddress, sponsorAssetTotal / 2, minFee, Some(sponsorAssetId), None).id
      nodes.waitForHeightAriseAndTxPresent(transferTxToSecondAddress)

      val sponsorId = sponsor.sponsorAsset(sponsor.keyPair, sponsorAssetId, baseFee = Token, fee = sponsorReducedFee).id
      nodes.waitForHeightAriseAndTxPresent(sponsorId)

      val height = nodes.waitForHeightArise()

      val txIds = txRequestsGen(50, sponsorAssetId)
      // The transfers spread over however many blocks the broadcasts take, and the block after the last one gets its 60% share
      val lastTxHeight = txIds.map(notMiner.waitForSettledTransaction).max
      nodes.waitForHeight(lastTxHeight + 2)

      // The block before `height` is only the baseline balance; it may still hold the sponsorship transaction
      val blockHeadersSeq = notMiner.blockHeadersSeq(height - 1, lastTxHeight.next)
      val transferBlocks  = blockHeadersSeq.drop(1)
      transferBlocks.map(_.transactionCount).sum shouldBe txIds.size
      transferBlocks.map(_.totalFee).sum shouldBe txIds.size * FeeValidation.FeeUnit * SmallFee / minSponsorFee

      val minerBalances = blockHeadersSeq.map(b => notMiner.debugStateAt(b.height)(b.generator))
      minerBalances.zip(blockHeadersSeq.map(_.totalFee)).sliding(2).foreach {
        case Seq((minerBalance1, blockFee1), (minerBalance2, blockFee2)) =>
          minerBalance2 should be(minerBalance1 + blockFee1 * 6 / 10 + blockFee2 * 4 / 10)

        case _ => ???
      }

      transferBlocks.foreach { header =>
        val realFee = notMiner.blockAt(header.height).transactions.map(tx => Sponsorship.toWaves(tx.fee, Token)).sum
        header.totalFee shouldBe realFee
      }
    }
  }

  import NodeConfigs.*
  override def nodeConfigs: Seq[Config] = Seq(
    BiggestMiner
      .quorum(0)
      .overrides("""waves.blockchain.custom.functionality {
                   |  blocks-for-feature-activation = 1
                   |  feature-check-blocks-period = 1
                   |}""".stripMargin),
    Default(1).notMiner,
    Default(2).notMiner
  ).map(_.preactivatedFeatures((BlockchainFeatures.BlockReward, Height(1000000))))
}

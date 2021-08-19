package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.{BaseFreeSpec, NodeConfigs}
import com.wavesplatform.state.Sponsorship
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.utils.ScorexLogging

class MicroblocksSponsoredFeeTestSuite extends BaseFreeSpec with ScorexLogging {

  private lazy val sponsor      = nodes(1)
  private val Token             = 100L
  private val sponsorAssetTotal = 100000 * Token
  private val minSponsorFee     = Token
  private val SmallFee          = Token + Token / 2

  private def secondAddress = nodes(2).address

  private def txRequestsGen(n: Int, sponsorAssetId: String): Unit = {
    1 to n map (_ => {
      sponsor.transfer(sponsor.keyPair, secondAddress, Token, fee = SmallFee, None, Some(sponsorAssetId))
    })
  }

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

      txRequestsGen(50, sponsorAssetId)
      nodes.waitForHeight(height + 2)

      val blockHeadersSeq = notMiner.blockHeadersSeq(height - 1, height + 2)

      val filteredBlocks = blockHeadersSeq
        .zip(blockHeadersSeq.drop(1))
        .withFilter(t => t._1.transactionCount != t._2.transactionCount)
        .map(_._1) :+ blockHeadersSeq.last

      val filteredBlocksFee        = filteredBlocks.map(b => b.transactionCount * FeeValidation.FeeUnit * SmallFee / minSponsorFee)
      val minerBalances: Seq[Long] = filteredBlocks.map(b => notMiner.debugStateAt(b.height)(b.generator))

      minerBalances.zip(filteredBlocksFee).sliding(2).foreach {
        case Seq((minerBalance1, blockFee1), (minerBalance2, blockFee2)) =>
          minerBalance2 should be(minerBalance1 + blockFee1 * 6 / 10 + blockFee2 * 4 / 10)

        case _ => ???
      }

      val block   = notMiner.blockAt(height)
      val realFee = block.transactions.map(tx => Sponsorship.toWaves(tx.fee, Token)).sum
      blockHeadersSeq(1).totalFee shouldBe realFee
    }
  }

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.raw("waves.blockchain.custom.functionality.blocks-for-feature-activation=1"))
      .overrideBase(_.raw("waves.blockchain.custom.functionality.feature-check-blocks-period=1"))
      .overrideBase(_.preactivatedFeatures((14, 1000000)))
      .withDefault(1)
      .withSpecial(2, _.nonMiner)
      .buildNonConflicting()

}

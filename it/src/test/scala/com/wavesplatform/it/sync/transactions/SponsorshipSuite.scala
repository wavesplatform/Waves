package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.it.{NodeConfigs, ReportingTestName}
import org.scalatest.{Assertions, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._

class SponsorshipSuite extends FreeSpec with NodesFromDocker with Matchers with ReportingTestName with CancelAfterFailure {

  val Waves       = 100000000L
  val Token       = 100L
  val TinyFee     = Token / 2
  val SmallFee    = Token + Token / 2
  val minWavesFee = 0.001.waves
  val LargeFee    = 10 * Token

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.raw("waves.blockchain.custom.functionality.blocks-for-feature-activation=1"))
      .overrideBase(_.raw("waves.blockchain.custom.functionality.feature-check-blocks-period=1"))
      .withDefault(1)
      .withSpecial(3, _.nonMiner)
      .buildNonConflicting()

  val miner   = nodes.head
  val sponsor = nodes(1)
  val alice   = nodes(2)
  val bob     = nodes(3)

  "Fee in sponsored asset works correctly" - {

    val sponsorWavesBalance = miner.accountBalances(sponsor.address)._2
    val sponsorAssetTotal   = 100 * Token
    val minerWavesBalance   = miner.accountBalances(miner.address)._2

    val sponsorAssetId =
      sponsor
        .issue(sponsor.address,
               "SponsoredAsset",
               "Created by Sponsorship Suite",
               sponsorAssetTotal,
               decimals = 2,
               reissuable = false,
               fee = 1 * Waves)
        .id
    nodes.waitForHeightAriseAndTxPresent(sponsorAssetId)

    val transferTxToAlice = sponsor.transfer(sponsor.address, alice.address, sponsorAssetTotal / 2, minWavesFee, Some(sponsorAssetId), None).id
    nodes.waitForHeightAriseAndTxPresent(transferTxToAlice)

    val sponsorId = sponsor.sponsorAsset(sponsor.address, sponsorAssetId, baseFee = Token, fee = 1 * Waves).id
    nodes.waitForHeightAriseAndTxPresent(sponsorId)
    val height = sponsor.waitForTransaction(sponsorId).height

    val assetDetailsDefault = sponsor.assetsDetails(sponsorAssetId)

    nodes.waitForHeightArise()

    val sponsorSecondId = sponsor.sponsorAsset(sponsor.address, sponsorAssetId, baseFee = 2 * Token, fee = 1 * Waves).id
    nodes.waitForHeightAriseAndTxPresent(sponsorSecondId)

    for (n <- nodes) n.rollback(height, false)

    nodes.waitForHeightArise()

    val a = miner.assetsDetails(sponsorAssetId)

    "minSponsoredAssetFee is returned to default value" in {
      assert(Some(a.minSponsoredAssetFee) == Some(assetDetailsDefault.minSponsoredAssetFee))
    }

  }
}

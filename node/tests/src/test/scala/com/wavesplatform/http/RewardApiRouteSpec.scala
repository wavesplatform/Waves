package com.wavesplatform.http

import com.wavesplatform.account.Address
import com.wavesplatform.api.http.RewardApiRoute
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.TxHelpers
import org.scalactic.source.Position
import play.api.libs.json.{JsObject, JsValue}

class RewardApiRouteSpec extends RouteSpec("/blockchain") with WithDomain {

  val daoAddress: Address        = TxHelpers.address(100)
  val xtnBuybackAddress: Address = TxHelpers.address(101)

  val settingsWithoutAddresses: WavesSettings = RideV6.copy(blockchainSettings =
    RideV6.blockchainSettings.copy(functionalitySettings =
      RideV6.blockchainSettings.functionalitySettings.copy(daoAddress = None, xtnBuybackAddress = None)
    )
  )
  val settingsWithOnlyDaoAddress: WavesSettings = RideV6.copy(blockchainSettings =
    RideV6.blockchainSettings.copy(functionalitySettings =
      RideV6.blockchainSettings.functionalitySettings.copy(daoAddress = Some(daoAddress.toString), xtnBuybackAddress = None)
    )
  )
  val settingsWithOnlyXtnBuybackAddress: WavesSettings = RideV6.copy(blockchainSettings =
    RideV6.blockchainSettings.copy(functionalitySettings =
      RideV6.blockchainSettings.functionalitySettings.copy(xtnBuybackAddress = Some(xtnBuybackAddress.toString), daoAddress = None)
    )
  )
  val settingsWithBothAddresses: WavesSettings = RideV6.copy(blockchainSettings =
    RideV6.blockchainSettings.copy(functionalitySettings =
      RideV6.blockchainSettings.functionalitySettings
        .copy(daoAddress = Some(daoAddress.toString), xtnBuybackAddress = Some(xtnBuybackAddress.toString))
    )
  )

  val blockRewardActivationHeight = 1
  val settingsWithVoteParams: WavesSettings = ConsensusImprovements
    .copy(blockchainSettings =
      ConsensusImprovements.blockchainSettings
        .copy(rewardsSettings =
          ConsensusImprovements.blockchainSettings.rewardsSettings.copy(term = 100, termAfterCappedRewardFeature = 50, votingInterval = 10)
        )
    )
    .setFeaturesHeight(BlockchainFeatures.BlockReward -> blockRewardActivationHeight, BlockchainFeatures.CappedReward -> 3)

  routePath("/rewards (NODE-855)") in {
    checkWithSettings(settingsWithoutAddresses)
    checkWithSettings(settingsWithOnlyDaoAddress)
    checkWithSettings(settingsWithOnlyXtnBuybackAddress)
    checkWithSettings(settingsWithBothAddresses)

    withDomain(settingsWithVoteParams) { d =>
      d.appendBlock()
      d.appendBlock()

      checkVoteParams(
        d,
        d.blockchain.settings.rewardsSettings.term,
        blockRewardActivationHeight + d.blockchain.settings.rewardsSettings.term - d.blockchain.settings.rewardsSettings.votingInterval,
        blockRewardActivationHeight + d.blockchain.settings.rewardsSettings.term - 1
      )

      d.appendBlock() // activation height, vote parameters should be changed
      checkVoteParams(
        d,
        d.blockchain.settings.rewardsSettings.termAfterCappedRewardFeature,
        blockRewardActivationHeight + d.blockchain.settings.rewardsSettings.termAfterCappedRewardFeature - d.blockchain.settings.rewardsSettings.votingInterval,
        blockRewardActivationHeight + d.blockchain.settings.rewardsSettings.termAfterCappedRewardFeature - 1
      )

      d.appendBlock()
      checkVoteParams(
        d,
        d.blockchain.settings.rewardsSettings.termAfterCappedRewardFeature,
        blockRewardActivationHeight + d.blockchain.settings.rewardsSettings.termAfterCappedRewardFeature - d.blockchain.settings.rewardsSettings.votingInterval,
        blockRewardActivationHeight + d.blockchain.settings.rewardsSettings.termAfterCappedRewardFeature - 1
      )
    }
  }

  routePath("/rewards/{height} (NODE-856)") in {
    checkWithSettings(settingsWithoutAddresses, Some(1))
    checkWithSettings(settingsWithOnlyDaoAddress, Some(1))
    checkWithSettings(settingsWithOnlyXtnBuybackAddress, Some(1))
    checkWithSettings(settingsWithBothAddresses, Some(1))

    withDomain(settingsWithVoteParams) { d =>
      d.appendBlock()
      d.appendBlock()
      d.appendBlock() // activation height, vote parameters should be changed
      d.appendBlock()

      checkVoteParams(
        d,
        d.blockchain.settings.rewardsSettings.term,
        blockRewardActivationHeight + d.blockchain.settings.rewardsSettings.term - d.blockchain.settings.rewardsSettings.votingInterval,
        blockRewardActivationHeight + d.blockchain.settings.rewardsSettings.term - 1,
        Some(2)
      )

      checkVoteParams(
        d,
        d.blockchain.settings.rewardsSettings.termAfterCappedRewardFeature,
        blockRewardActivationHeight + d.blockchain.settings.rewardsSettings.termAfterCappedRewardFeature - d.blockchain.settings.rewardsSettings.votingInterval,
        blockRewardActivationHeight + d.blockchain.settings.rewardsSettings.termAfterCappedRewardFeature - 1,
        Some(3)
      )

      checkVoteParams(
        d,
        d.blockchain.settings.rewardsSettings.termAfterCappedRewardFeature,
        blockRewardActivationHeight + d.blockchain.settings.rewardsSettings.termAfterCappedRewardFeature - d.blockchain.settings.rewardsSettings.votingInterval,
        blockRewardActivationHeight + d.blockchain.settings.rewardsSettings.termAfterCappedRewardFeature - 1,
        Some(4)
      )
    }
  }

  private def checkWithSettings(settings: WavesSettings, height: Option[Int] = None) =
    withDomain(settings) { d =>
      val route = RewardApiRoute(d.blockchain).route

      d.appendBlock()

      val pathSuffix = height.fold("")(h => s"/$h")

      Get(routePath(s"/rewards$pathSuffix")) ~> route ~> check {
        responseAs[JsValue] should matchJson(expectedResponse(d))
      }
    }

  private def expectedResponse(d: Domain) =
    s"""
       |{
       |  "height" : 1,
       |  "totalWavesAmount" : ${d.blockchain.settings.genesisSettings.initialBalance + d.blockchain.settings.rewardsSettings.initial},
       |  "currentReward" : ${d.blockchain.settings.rewardsSettings.initial},
       |  "minIncrement" : ${d.blockchain.settings.rewardsSettings.minIncrement},
       |  "term" : ${d.blockchain.settings.rewardsSettings.term},
       |  "nextCheck" : ${d.blockchain.settings.rewardsSettings.nearestTermEnd(0, 1, modifyTerm = false)},
       |  "votingIntervalStart" : ${d.blockchain.settings.rewardsSettings
      .nearestTermEnd(0, 1, modifyTerm = false) - d.blockchain.settings.rewardsSettings.votingInterval + 1},
       |  "votingInterval" : ${d.blockchain.settings.rewardsSettings.votingInterval},
       |  "votingThreshold" : ${d.blockchain.settings.rewardsSettings.votingInterval / 2 + 1},
       |  "votes" : {
       |    "increase" : 0,
       |    "decrease" : 0
       |  },
       |  "daoAddress" : ${d.blockchain.settings.functionalitySettings.daoAddress.fold("null")(addr => s"\"$addr\"")},
       |  "xtnBuybackAddress" : ${d.blockchain.settings.functionalitySettings.xtnBuybackAddress.fold("null")(addr => s"\"$addr\"")}
       |}
       |""".stripMargin

  private def checkVoteParams(d: Domain, expectedTerm: Int, expectedVotingIntervalStart: Int, expectedNextCheck: Int, height: Option[Int] = None) = {
    val route      = RewardApiRoute(d.blockchain).route
    val pathSuffix = height.fold("")(h => s"/$h")

    Get(routePath(s"/rewards$pathSuffix")) ~> route ~> check {
      val response = responseAs[JsValue]
      (response \ "term").as[Int] shouldBe expectedTerm
      (response \ "votingIntervalStart").as[Int] shouldBe expectedVotingIntervalStart
      (response \ "nextCheck").as[Int] shouldBe expectedNextCheck
    }
  }

  "Boost block reward feature changes API response" in {
    val miner      = TxHelpers.signer(3001)
    val daoAddress = TxHelpers.address(3002)
    val xtnAddress = TxHelpers.address(3003)

    val settings = DomainPresets.ConsensusImprovements
      .setFeaturesHeight(
        BlockchainFeatures.BlockRewardDistribution -> 0,
        BlockchainFeatures.CappedReward            -> 0,
        BlockchainFeatures.BoostBlockReward        -> 5,
        BlockchainFeatures.CeaseXtnBuyback         -> 0
      )
      .configure(fs =>
        fs.copy(
          xtnBuybackRewardPeriod = 10,
          blockRewardBoostPeriod = 10,
          xtnBuybackAddress = Some(xtnAddress.toString),
          daoAddress = Some(daoAddress.toString)
        )
      )

    withDomain(settings, Seq(AddrWithBalance(miner.toAddress, 100_000.waves))) { d =>
      val route = new RewardApiRoute(d.blockchain).route

      def checkRewardAndShares(height: Int, expectedReward: Long, expectedMinerShare: Long, expectedDaoShare: Long, expectedXtnShare: Option[Long])(
          implicit pos: Position
      ): Unit = {

        val path = routePath(s"/rewards/$height")
        withClue(path) {
          Get(path) ~> route ~> check {
            val jsonResp = responseAs[JsObject]
            withClue(" reward:") {
              (jsonResp \ "currentReward").as[Long] shouldBe expectedReward
            }
          }
        }
      }

      (1 to 3).foreach(_ => d.appendKeyBlock(miner))
      d.blockchain.height shouldBe 4
      (1 to 3).foreach { h =>
        checkRewardAndShares(h + 1, 6.waves, 2.waves, 2.waves, Some(2.waves))
      }

      // reward boost activation
      (1 to 5).foreach(_ => d.appendKeyBlock(miner))
      (1 to 5).foreach { h =>
        checkRewardAndShares(h + 4, 60.waves, 20.waves, 20.waves, Some(20.waves))
      }

      // cease XTN buyback
      (1 to 5).foreach(_ => d.appendKeyBlock(miner))
      (1 to 5).foreach { h =>
        checkRewardAndShares(h + 9, 60.waves, 40.waves, 20.waves, None)
      }

      d.appendKeyBlock(miner)
      d.blockchain.height shouldBe 15
      checkRewardAndShares(15, 6.waves, 4.waves, 2.waves, None)
    }
  }
}

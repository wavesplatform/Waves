package com.wavesplatform.http

import com.wavesplatform.account.Address
import com.wavesplatform.api.http.RewardApiRoute
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.TxHelpers
import play.api.libs.json.JsValue

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
}

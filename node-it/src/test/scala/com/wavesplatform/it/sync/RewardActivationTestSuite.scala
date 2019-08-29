package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures}
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.activation.ActivationStatusRequest
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.settings.Constants
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers, OptionValues}

class RewardActivationTestSuite
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with NodesFromDocker
    with ActivationStatusRequest
    with ReportingTestName
    with OptionValues {
  import RewardActivationTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  "reward changes accordingly node's votes" in {

    nodes.waitForHeight(activationHeight)
    val featureInfo = nodes.map(_.featureActivationStatus(BlockchainFeatures.BlockReward.id))
    featureInfo.foreach { si =>
      si.description shouldBe BlockchainFeatures.BlockReward.description
      withClue("blockchainStatus") {
        si.blockchainStatus shouldBe BlockchainFeatureStatus.Activated
      }
    }

    val firstVoteHeight = activationHeight + rewardPeriod
    nodes.waitForHeight(activationHeight + 1)
    val statusInfo = nodes.map(_.rewardStatus(activationHeight + 1))
    statusInfo.foreach { ri =>
      ri.value.reward should be(firstReward)
      ri.value.periodStart should be(activationHeight + 1)
      ri.value.periodEnd should be(firstVoteHeight)
      ri.value.votingPeriodStart should be(firstVoteHeight - rewardVotingPeriod + 1)
      ri.value.isVotingPeriod should be(false)
    }

    val firstVoteStartHeight = firstVoteHeight - rewardVotingPeriod + 1
    nodes.waitForHeight(firstVoteStartHeight)
    val firstVotingPeriodStatusInfo = nodes.map(_.rewardStatus(firstVoteStartHeight))
    firstVotingPeriodStatusInfo.foreach { ri =>
      ri.value.reward should be(firstReward)
      ri.value.periodStart should be(activationHeight + 1)
      ri.value.periodEnd should be(firstVoteHeight)
      ri.value.votingPeriodStart should be(firstVoteHeight - rewardVotingPeriod + 1)
      ri.value.isVotingPeriod should be(true)
    }

    nodes.waitForHeight(firstVoteHeight)
    nodes.foreach(_.blockSeq(firstVoteHeight - rewardVotingPeriod, firstVoteHeight).foreach { b =>
      b.reward.value should be((rewardSupport * Constants.UnitsInWave).toLong)
    })

    nodes.waitForHeight(firstVoteHeight + 1)
    val firstVoteStatusInfo = nodes.map(_.rewardStatus(firstVoteHeight + 1))
    firstVoteStatusInfo.foreach { ri =>
      ri.value.reward should be(firstReward + rewardStep)
      ri.value.periodStart should be(firstVoteHeight + 1)
      ri.value.periodEnd should be(firstVoteHeight + rewardPeriod)
      ri.value.votingPeriodStart should be(firstVoteHeight + rewardPeriod - rewardVotingPeriod + 1)
      ri.value.isVotingPeriod should be(false)
    }
  }
}

object RewardActivationTestSuite {
  private val activationHeight   = 4
  private val rewardSupport      = 6.5
  private val rewardStep         = 50000000
  private val firstReward        = 600000000
  private val rewardPeriod       = 8
  private val rewardVotingPeriod = 4

  val config: Config = ConfigFactory.parseString(
    s"""waves {
       |  blockchain.custom.functionality {
       |    pre-activated-features = {
       |      ${BlockchainFeatures.BlockReward.id} = $activationHeight
       |    }
       |    block-reward-settings {
       |      min-reward = 0
       |      first-reward = $firstReward
       |      reward-step = $rewardStep
       |      reward-period = $rewardPeriod
       |      reward-voting-period = $rewardVotingPeriod
       |    }
       |  }
       |  reward.supported = $rewardSupport
       |  miner.quorum = 1
       |}""".stripMargin
  )

  val Configs: Seq[Config] = Seq(
    config.withFallback(Default.head),
    config.withFallback(Default(1))
  )
}

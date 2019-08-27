package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.api.NodeFeatureStatus
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.activation.ActivationStatusRequest
import com.wavesplatform.it.transactions.NodesFromDocker
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
      assertActivatedStatus(si, activationHeight, NodeFeatureStatus.Implemented)
    }

    val statusInfo = nodes.map(_.rewardStatus)
    statusInfo.foreach { ri =>
      ri.value.reward should be(firstReward)
      ri.value.isVotingPeriod should be(false)
    }

    nodes.waitForHeight(activationHeight + firstRewardPeriod - rewardVotingPeriod)
    val firstVotingPeriodStatusInfo = nodes.map(_.rewardStatus)
    firstVotingPeriodStatusInfo.foreach { ri =>
      ri.value.reward should be(firstReward)
      ri.value.isVotingPeriod should be(true)
      ri.value.blocksBeforeNextVote should be <= rewardVotingPeriod
    }

    val firstVoteHeight = activationHeight + firstRewardPeriod

    nodes.waitForHeight(firstVoteHeight + 1)

    nodes.foreach(_.blockSeq(firstVoteHeight - rewardVotingPeriod, firstVoteHeight).foreach { b =>
      b.reward.value should be(rewardSupport)
    })

    val firstVoteStatusInfo = nodes.map(_.rewardStatus)
    firstVoteStatusInfo.foreach { ri =>
      ri.value.reward should be(firstReward + rewardStep * rewardSupport)
    }

    nodes.waitForHeight(firstVoteHeight + rewardPeriod + 1)
    val nextVoteStatusInfo = nodes.map(_.rewardStatus)
    nextVoteStatusInfo.foreach { ri =>
      ri.value.reward should be(firstReward + rewardStep * (rewardSupport + 1))
    }

    nodes.waitForHeight(firstVoteHeight + rewardPeriod * 2 + 1)
    val lastVoteStatusInfo = nodes.map(_.rewardStatus)
    lastVoteStatusInfo.foreach { ri =>
      ri.value.reward should be(firstReward + rewardStep * (rewardSupport + 1))
    }
  }
}

object RewardActivationTestSuite {
  private val activationHeight   = 4
  private val rewardSupport      = 3.toByte
  private val rewardStep         = 25000000
  private val firstReward        = 600000000
  private val firstRewardPeriod  = 10
  private val rewardPeriod       = 8
  private val rewardVotingPeriod = 4

  val config: Config = ConfigFactory.parseString(
    s"""waves {
       |  blockchain.custom.functionality {
       |    pre-activated-features = {
       |      14 = $activationHeight
       |    }
       |    block-reward-settings {
       |      min-reward = 0
       |      max-reward = ${firstReward + rewardStep * 4}
       |      first-reward = $firstReward
       |      reward-step = $rewardStep
       |      first-reward-period = $firstRewardPeriod
       |      reward-period = $rewardPeriod
       |      reward-voting-period = $rewardVotingPeriod
       |    }
       |  }
       |  features.reward = $rewardSupport
       |  miner.quorum = 1
       |}""".stripMargin
  )

  val Configs: Seq[Config] = Seq(
    config.withFallback(Default.head),
    config.withFallback(Default(1))
  )
}

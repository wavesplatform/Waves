package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.api.http.ApiError.CustomValidationError
import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures}
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.activation.ActivationStatusRequest
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.settings.Constants
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers, OptionValues}

import scala.concurrent.duration._

class RewardsTestSuite
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with NodesFromDocker
    with ActivationStatusRequest
    with ReportingTestName
    with OptionValues {
  import RewardsTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private val miner = nodes.head

  "reward changes accordingly node's votes and miner's balance changes by reward amount after block generation" - {
    val initMinerBalance = miner.balanceDetails(miner.address).available
    val initialAmount = BigInt(Constants.TotalWaves) * BigInt(Constants.UnitsInWave)

    "when miner votes for increase" in {

      assertApiError(miner.rewardStatus(1), CustomValidationError("Block reward feature is not activated yet"))
      miner.waitForHeight(activationHeight - 1, 5.minutes)
      miner.balanceDetails(miner.address).available shouldBe initMinerBalance
      miner.waitForHeight(activationHeight)

      val featureInfo = miner.featureActivationStatus(BlockchainFeatures.BlockReward.id)
      featureInfo.description shouldBe BlockchainFeatures.BlockReward.description
      featureInfo.blockchainStatus shouldBe BlockchainFeatureStatus.Activated

      val minerBalanceAtActivationHeight = miner.balanceDetails(miner.address).available
      minerBalanceAtActivationHeight shouldBe initMinerBalance + miner.rewardStatus(miner.height).currentReward

      val rewardAtActivation = miner.rewardStatus(activationHeight)
      rewardAtActivation.currentReward shouldBe initial
      rewardAtActivation.minIncrement shouldBe minIncrement
      rewardAtActivation.term shouldBe term
      rewardAtActivation.nextCheck shouldBe activationHeight + term - 1
      rewardAtActivation.votingIntervalStart shouldBe activationHeight + term - votingInterval
      rewardAtActivation.votingThreshold shouldBe votingInterval / 2 + 1
      rewardAtActivation.votes.increase shouldBe 0
      rewardAtActivation.votes.decrease shouldBe 0
      rewardAtActivation.totalWavesAmount shouldBe initialAmount + initial

      miner.waitForHeight(activationHeight + 1) // 5
      miner.balanceDetails(miner.address).available shouldBe minerBalanceAtActivationHeight + miner.rewardStatus(miner.height).currentReward

      val votingStartHeight = activationHeight + term - votingInterval
      miner.waitForHeight(votingStartHeight, 2.minutes) // 8
      val rewardAfterFirstVote = miner.rewardStatus(votingStartHeight)
      rewardAfterFirstVote.currentReward shouldBe initial
      rewardAfterFirstVote.minIncrement shouldBe minIncrement
      rewardAfterFirstVote.term shouldBe term
      rewardAfterFirstVote.nextCheck shouldBe activationHeight + term - 1
      rewardAfterFirstVote.votingIntervalStart shouldBe activationHeight + term - votingInterval
      rewardAfterFirstVote.votingThreshold shouldBe votingInterval / 2 + 1
      rewardAfterFirstVote.votes.increase shouldBe 1
      rewardAfterFirstVote.votes.decrease shouldBe 0
      rewardAfterFirstVote.totalWavesAmount shouldBe initialAmount + BigInt(initial) * BigInt(votingStartHeight - activationHeight + 1)

      val termEndHeight = activationHeight + term
      val newReward = initial + minIncrement
      val amountAfterTerm = initialAmount + BigInt(initial) * BigInt(term) + newReward

      miner.waitForHeight(termEndHeight - 1, 2.minutes) // 11
      miner.rewardStatus(miner.height).currentReward shouldBe initial
      val minerBalanceBeforeTermEnd = miner.balanceDetails(miner.address).available
      minerBalanceBeforeTermEnd shouldBe minerBalanceAtActivationHeight + (termEndHeight - activationHeight - 1) * miner.rewardStatus(miner.height).currentReward

      miner.waitForHeight(termEndHeight) // 12

      val rewardAtTermEnd = miner.rewardStatus(termEndHeight)
      rewardAtTermEnd.currentReward shouldBe newReward
      rewardAtTermEnd.minIncrement shouldBe minIncrement
      rewardAtTermEnd.term shouldBe term
      rewardAtTermEnd.nextCheck shouldBe termEndHeight + term - 1
      rewardAtTermEnd.votingIntervalStart shouldBe termEndHeight + term - votingInterval
      rewardAtTermEnd.votingThreshold shouldBe votingInterval / 2 + 1
      rewardAtTermEnd.votes.increase shouldBe 0
      rewardAtTermEnd.votes.decrease shouldBe 0
      rewardAtTermEnd.totalWavesAmount shouldBe amountAfterTerm
      val minerBalanceAtTermEndHeight = miner.balanceDetails(miner.address).available
      minerBalanceAtTermEndHeight shouldBe minerBalanceBeforeTermEnd + miner.rewardStatus(miner.height).currentReward

      miner.waitForHeight(termEndHeight + 1) // 13
      miner.rewardStatus(miner.height).currentReward shouldBe newReward
      miner.balanceDetails(miner.address).available shouldBe minerBalanceAtTermEndHeight + miner.rewardStatus(miner.height).currentReward

      val secondVotingStartHeightPlusTwo = termEndHeight + term - votingInterval + 2

      miner.waitForHeight(secondVotingStartHeightPlusTwo, 5.minutes) // 18
      val rewardSecVoting = miner.rewardStatus(secondVotingStartHeightPlusTwo)
      rewardSecVoting.currentReward shouldBe newReward
      rewardSecVoting.minIncrement shouldBe minIncrement
      rewardSecVoting.term shouldBe term
      rewardSecVoting.nextCheck shouldBe termEndHeight + term - 1
      rewardSecVoting.votingIntervalStart shouldBe termEndHeight + term - votingInterval
      rewardSecVoting.votingThreshold shouldBe votingInterval / 2 + 1
      rewardSecVoting.votes.increase shouldBe 3
      rewardSecVoting.votes.decrease shouldBe 0
      rewardSecVoting.totalWavesAmount shouldBe amountAfterTerm + newReward * BigInt(secondVotingStartHeightPlusTwo - termEndHeight)
    }
    "when miner votes for decrease" in {
      docker.restartNode(dockerNodes().head, configWithDecreasedDesired)
      if (miner.height != 1) nodes.rollback(2, false)

      miner.waitForHeight(activationHeight, 2.minutes)
      val minerBalanceAtActivationHeight = miner.balanceDetails(miner.address).available
      minerBalanceAtActivationHeight shouldBe initMinerBalance + miner.rewardStatus(miner.height).currentReward

      val rewardAtActivation = miner.rewardStatus(activationHeight)
      rewardAtActivation.currentReward shouldBe initial
      rewardAtActivation.minIncrement shouldBe minIncrement
      rewardAtActivation.term shouldBe term
      rewardAtActivation.nextCheck shouldBe activationHeight + term - 1
      rewardAtActivation.votingIntervalStart shouldBe activationHeight + term - votingInterval
      rewardAtActivation.votingThreshold shouldBe votingInterval / 2 + 1
      rewardAtActivation.votes.increase shouldBe 0
      rewardAtActivation.votes.decrease shouldBe 0
      rewardAtActivation.totalWavesAmount shouldBe initialAmount + initial

      val termEndHeight = activationHeight + term
      val newReward = initial - minIncrement
      val amountAfterTerm = initialAmount + BigInt(initial) * BigInt(term) + newReward

      miner.waitForHeight(termEndHeight - 1, 2.minutes)
      val minerBalanceBeforeTermEnd = miner.balanceDetails(miner.address).available
      minerBalanceBeforeTermEnd shouldBe minerBalanceAtActivationHeight + (termEndHeight - activationHeight - 1) * miner.rewardStatus(miner.height).currentReward

      miner.waitForHeight(termEndHeight)
      val rewardAtTermEnd = miner.rewardStatus(termEndHeight)
      rewardAtTermEnd.currentReward shouldBe newReward
      rewardAtTermEnd.minIncrement shouldBe minIncrement
      rewardAtTermEnd.term shouldBe term
      rewardAtTermEnd.nextCheck shouldBe termEndHeight + term - 1
      rewardAtTermEnd.votingIntervalStart shouldBe termEndHeight + term - votingInterval
      rewardAtTermEnd.votingThreshold shouldBe votingInterval / 2 + 1
      rewardAtTermEnd.votes.increase shouldBe 0
      rewardAtTermEnd.votes.decrease shouldBe 0
      rewardAtTermEnd.totalWavesAmount shouldBe amountAfterTerm
      val minerBalanceAtTermEnd = miner.balanceDetails(miner.address).available
      minerBalanceAtTermEnd shouldBe minerBalanceBeforeTermEnd + miner.rewardStatus(miner.height).currentReward

      miner.waitForHeight(termEndHeight + 1)
      miner.rewardStatus(termEndHeight + 1).currentReward shouldBe newReward
      miner.balanceDetails(miner.address).available shouldBe minerBalanceAtTermEnd + miner.rewardStatus(miner.height).currentReward

      val secondVotingStartHeightPlusTwo = termEndHeight + term - votingInterval + 2
      miner.waitForHeight(secondVotingStartHeightPlusTwo, 5.minutes) // 18
      val rewardSecVoting = miner.rewardStatus(secondVotingStartHeightPlusTwo)
      rewardSecVoting.currentReward shouldBe newReward
      rewardSecVoting.minIncrement shouldBe minIncrement
      rewardSecVoting.term shouldBe term
      rewardSecVoting.nextCheck shouldBe termEndHeight + term - 1
      rewardSecVoting.votingIntervalStart shouldBe termEndHeight + term - votingInterval
      rewardSecVoting.votingThreshold shouldBe votingInterval / 2 + 1
      rewardSecVoting.votes.increase shouldBe 0
      rewardSecVoting.votes.decrease shouldBe 3
      rewardSecVoting.totalWavesAmount shouldBe amountAfterTerm + newReward * BigInt(secondVotingStartHeightPlusTwo - termEndHeight)
    }
  }
}

object RewardsTestSuite {
  private val activationHeight = 4
  private val increasedDesired = 750000000
  private val decreasedDesired = 450000000
  private val minIncrement     = 50000000
  private val initial          = 600000000
  private val term             = 8
  private val votingInterval   = 4

  val configWithIncreasedDesired: Config = ConfigFactory.parseString(
    s"""waves {
       |  blockchain.custom.functionality {
       |    pre-activated-features = {
       |      ${BlockchainFeatures.BlockReward.id} = $activationHeight
       |    }
       |  }
       |  blockchain.custom.rewards {
       |    term = $term
       |    initial = $initial
       |    min-increment = $minIncrement
       |    voting-interval = $votingInterval
       |  }
       |  rewards.desired = $increasedDesired
       |  miner.quorum = 0
       |}""".stripMargin
  )

  val configWithDecreasedDesired: Config = ConfigFactory.parseString(
    s"""waves {
       |  blockchain.custom.functionality {
       |    pre-activated-features = {
       |      ${BlockchainFeatures.BlockReward.id} = $activationHeight
       |    }
       |  }
       |  blockchain.custom.rewards {
       |    term = $term
       |    initial = $initial
       |    min-increment = $minIncrement
       |    voting-interval = $votingInterval
       |  }
       |  rewards.desired = $decreasedDesired
       |  miner.quorum = 0
       |}""".stripMargin
  )

  val Configs: Seq[Config] = Seq(
    configWithIncreasedDesired.withFallback(Default.head)
  )
}

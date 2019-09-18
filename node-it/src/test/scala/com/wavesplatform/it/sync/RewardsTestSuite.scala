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

  private val firstMiner = nodes.head
  private val secondMiner = nodes.last

  "reward changes accordingly node's votes" in {

    val initialAmount = BigInt(Constants.TotalWaves) * BigInt(Constants.UnitsInWave)

    val initFirstMinerBalance = firstMiner.balanceDetails(firstMiner.address).available
    val initSecondMinerBalance = firstMiner.balanceDetails(secondMiner.address).available

    assertBadRequestAndMessage(firstMiner.rewardStatus(1),"Block reward feature is not activated yet",400)
    nodes.waitForHeight(activationHeight - 1)
    firstMiner.balanceDetails(firstMiner.address).available shouldBe initFirstMinerBalance
    firstMiner.balanceDetails(secondMiner.address).available shouldBe initSecondMinerBalance

    nodes.waitForHeight(activationHeight)
    val featureInfo = nodes.map(_.featureActivationStatus(BlockchainFeatures.BlockReward.id))
    featureInfo.foreach { si =>
      si.description shouldBe BlockchainFeatures.BlockReward.description
      withClue("blockchainStatus") {
        si.blockchainStatus shouldBe BlockchainFeatureStatus.Activated
      }
    }

    nodes.waitForHeight(activationHeight)
    nodes.map(_.rewardStatus(activationHeight)).foreach { ri => // 4
      ri.currentReward should be(initial)
      ri.minIncrement should be(minIncrement)
      ri.term should be(term)
      ri.nextCheck should be(activationHeight + term - 1)
      ri.votingIntervalStart should be(activationHeight + term - votingInterval)
      ri.votingThreshold should be(votingInterval / 2 + 1)
      ri.votes.increase should be(0)
      ri.votes.decrease should be(0)
      ri.totalWavesAmount should be(initialAmount + initial)
    }

    val votingStartHeight = activationHeight + term - votingInterval
    nodes.waitForHeight(votingStartHeight)
    nodes.map(_.rewardStatus(votingStartHeight)).foreach { ri => // 8
      ri.currentReward should be(initial)
      ri.minIncrement should be(minIncrement)
      ri.term should be(term)
      ri.nextCheck should be(activationHeight + term - 1)
      ri.votingIntervalStart should be(activationHeight + term - votingInterval)
      ri.votingThreshold should be(votingInterval / 2 + 1)
      ri.votes.increase should be(1)
      ri.votes.decrease should be(0)
      ri.totalWavesAmount should be(initialAmount + BigInt(initial) * BigInt(votingStartHeight - activationHeight + 1))
    }

    val termEndHeight   = activationHeight + term
    val newReward       = initial + minIncrement
    val amountAfterTerm = initialAmount + BigInt(initial) * BigInt(term) + newReward

    nodes.waitForHeight(termEndHeight)
    nodes.map(_.rewardStatus(termEndHeight)).foreach { ri => // 12
      ri.currentReward should be(newReward)
      ri.minIncrement should be(minIncrement)
      ri.term should be(term)
      ri.nextCheck should be(termEndHeight + term - 1)
      ri.votingIntervalStart should be(termEndHeight + term - votingInterval)
      ri.votingThreshold should be(votingInterval / 2 + 1)
      ri.votes.increase should be(0)
      ri.votes.decrease should be(0)
      ri.totalWavesAmount should be(amountAfterTerm)
    }

    val secondVotingStartHeightPlusTwo = termEndHeight + term - votingInterval + 2

    nodes.waitForHeight(secondVotingStartHeightPlusTwo)
    nodes.map(_.rewardStatus(secondVotingStartHeightPlusTwo)).foreach { ri => // 18
      ri.currentReward should be(newReward)
      ri.minIncrement should be(minIncrement)
      ri.term should be(term)
      ri.nextCheck should be(termEndHeight + term - 1)
      ri.votingIntervalStart should be(termEndHeight + term - votingInterval)
      ri.votingThreshold should be(votingInterval / 2 + 1)
      ri.votes.increase should be(3)
      ri.votes.decrease should be(0)
      ri.totalWavesAmount should be(amountAfterTerm + newReward * BigInt(secondVotingStartHeightPlusTwo - termEndHeight))
    }
  }

  "miner's balance increases as he generates block" in {
    nodes.last.close()
    val minerBalanceBefore = firstMiner.balanceDetails(firstMiner.address).available
    val currentHeight = firstMiner.height
    firstMiner.waitForHeight(currentHeight + 1)
    firstMiner.balanceDetails(firstMiner.address).available shouldBe minerBalanceBefore + firstMiner.rewardStatus(firstMiner.height).currentReward
  }
}

object RewardsTestSuite {
  private val activationHeight = 4
  private val target           = 750000000
  private val minIncrement     = 50000000
  private val initial          = 600000000
  private val term             = 8
  private val votingInterval   = 4

  val config: Config = ConfigFactory.parseString(
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
       |  rewards.desired = $target
       |  miner.quorum = 1
       |}""".stripMargin
  )

  val Configs: Seq[Config] = Seq(
    config.withFallback(Default.head),
    config.withFallback(Default(1))
  )
}

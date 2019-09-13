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

  "reward changes accordingly node's votes" in {

    val initialAmount = BigInt(Constants.TotalWaves) * BigInt(Constants.UnitsInWave)

    nodes.waitForHeight(activationHeight)
    val featureInfo = nodes.map(_.featureActivationStatus(BlockchainFeatures.BlockReward.id))
    featureInfo.foreach { si =>
      si.description shouldBe BlockchainFeatures.BlockReward.description
      withClue("blockchainStatus") {
        si.blockchainStatus shouldBe BlockchainFeatureStatus.Activated
      }
    }

    nodes.waitForHeight(activationHeight + 1)
    nodes.map(_.rewardStatus(activationHeight)).foreach { ri => // 4
      ri.currentReward should be(initial)
      ri.minIncrement should be(minIncrement)
      ri.term should be(term)
      ri.nextCheck should be(activationHeight + term)
      ri.votingIntervalStart should be(activationHeight + term - votingInterval + 1)
      ri.votingThreshold should be(votingInterval / 2 + 1)
      ri.votes.increase should be(0)
      ri.votes.decrease should be(0)
      ri.totalWavesAmount should be(initialAmount + initial)
    }

    val votingStartHeight = activationHeight + term - votingInterval + 1
    nodes.waitForHeight(votingStartHeight + 1)
    nodes.map(_.rewardStatus(votingStartHeight)).foreach { ri => // 9
      ri.currentReward should be(initial)
      ri.minIncrement should be(minIncrement)
      ri.term should be(term)
      ri.nextCheck should be(activationHeight + term)
      ri.votingIntervalStart should be(activationHeight + term - votingInterval + 1)
      ri.votingThreshold should be(votingInterval / 2 + 1)
      ri.votes.increase should be(1)
      ri.votes.decrease should be(0)
      ri.totalWavesAmount should be(initialAmount + BigInt(initial) * BigInt(votingStartHeight - activationHeight + 1))
    }

    val termEndHeight   = activationHeight + term
    val newReward       = initial + minIncrement
    val amountAfterTerm = initialAmount + BigInt(initial) * BigInt(term) + newReward

    nodes.waitForHeight(termEndHeight + 1)
    nodes.map(_.rewardStatus(termEndHeight)).foreach { ri => // 12
      ri.currentReward should be(newReward)
      ri.minIncrement should be(minIncrement)
      ri.term should be(term)
      ri.nextCheck should be(termEndHeight + term)
      ri.votingIntervalStart should be(termEndHeight + term - votingInterval + 1)
      ri.votingThreshold should be(votingInterval / 2 + 1)
      ri.votes.increase should be(0)
      ri.votes.decrease should be(0)
      ri.totalWavesAmount should be(amountAfterTerm)
    }

    val secondVotingStartHeight = termEndHeight + term - votingInterval + 1

    nodes.waitForHeight(secondVotingStartHeight + 1)
    nodes.map(_.rewardStatus(secondVotingStartHeight)).foreach { ri => // 17
      ri.currentReward should be(newReward)
      ri.minIncrement should be(minIncrement)
      ri.term should be(term)
      ri.nextCheck should be(termEndHeight + term)
      ri.votingIntervalStart should be(termEndHeight + term - votingInterval + 1)
      ri.votingThreshold should be(votingInterval / 2 + 1)
      ri.votes.increase should be(1)
      ri.votes.decrease should be(0)
      ri.totalWavesAmount should be(amountAfterTerm + newReward * BigInt(secondVotingStartHeight - termEndHeight))
    }
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
       |  rewards.target = $target
       |  miner.quorum = 1
       |}""".stripMargin
  )

  val Configs: Seq[Config] = Seq(
    config.withFallback(Default.head),
    config.withFallback(Default(1))
  )
}

package com.wavesplatform.it.sync.finalization

import com.typesafe.config.Config
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.api.*
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.{BaseFreeSpec, NodeConfigs}
import com.wavesplatform.state.Height
import com.wavesplatform.test.NumericExt
import com.wavesplatform.utils.ScorexLogging
import org.scalatest.OptionValues

import scala.concurrent.duration.DurationInt

class TwoNodesFinalizationTestSuite extends BaseFreeSpec, OptionValues, ScorexLogging {
  import NodeConfigs.*
  override protected def nodeConfigs: Seq[Config] =
    Seq(Miners.head, Miners(3)).map(
      _.preactivatedFeatures(BlockchainFeatures.DeterministicFinality)
        .overrides("waves.waves.blockchain.custom.functionality.min-block-time = 10s")
        .quorum(1)
    )

  private def node1 = dockerNodes().head
  private def node2 = dockerNodes().last

  private lazy val miner1Acc  = node1.keyPair
  private lazy val miner1Addr = node1.address

  private lazy val miner2Addr = node2.address

  "finalization activated and works" in {
    val period1 = node1.currentGenerationPeriod.value.next

    step("Commit to generation")
    val commitTxn1 = node1.signCommitToGenerationRequest(miner1Addr)
    val commitTxn2 = node2.signCommitToGenerationRequest(miner2Addr)

    node2.broadcastRequest(commitTxn1)
    node2.broadcastRequest(commitTxn2)

    node1.waitForGenerationPeriod(period1)

    step("Generators")
    isolated {
      val generators = node1.generators(period1.start)
      generators.size shouldBe 2
      generators.map(ge => ge.address -> ge.transactionId) should contain theSameElementsAs Seq(
        miner1Addr -> commitTxn1.id,
        miner2Addr -> commitTxn2.id
      )
    }

    step("Finalized height checks")
    val deadline               = 2.minutes.fromNow
    var finalizedHeight1       = node1.finalizedHeight
    val waitingFinalizedHeight = finalizedHeight1 + 2

    var done = false
    while (!done && deadline.hasTimeLeft()) {
      val currHeight = node1.height
      if (currHeight > waitingFinalizedHeight + 2)
        fail(
          s"Finalization height doesn't rise: height=$currHeight, waiting for finalized height=$waitingFinalizedHeight, last finalized height=$finalizedHeight1"
        )

      // We need at least one transaction, otherwise there won't be a microblock, thus no voting, no finalization
      node1.transfer(miner1Acc, miner2Addr, 1.waves, waitForTx = true)

      val updatedFinalizedHeight = node1.finalizedHeight
      if (updatedFinalizedHeight < finalizedHeight1)
        fail(s"Finalized height $updatedFinalizedHeight became lower than the previous $finalizedHeight1")
      else if (updatedFinalizedHeight != finalizedHeight1)
        log.debug(s"New finalized height: $finalizedHeight1 -> $updatedFinalizedHeight")

      finalizedHeight1 = updatedFinalizedHeight
      done = finalizedHeight1 >= waitingFinalizedHeight
    }
  }
}

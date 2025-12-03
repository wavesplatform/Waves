package com.wavesplatform.it.sync.finalization

import com.typesafe.config.Config
import com.wavesplatform.api.http.requests.CommitToGenerationRequest
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.api.*
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.{BaseFreeSpec, NodeConfigs}
import com.wavesplatform.state.Height
import com.wavesplatform.test.NumericExt
import org.scalatest.OptionValues

import scala.concurrent.duration.DurationInt

class OneNodeFinalizationTestSuite extends BaseFreeSpec with OptionValues {
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.DeterministicFinality.id, Height(0))))
      .withDefault(1)
      .buildNonConflicting()

  private def node            = dockerNodes().last
  private lazy val miner1Acc  = node.keyPair
  private lazy val miner1Addr = node.address

  "finalization activated and works" in {
    val miner2Acc, miner3Acc = node.createKeyPairServerSide()
    val miner2Addr           = miner2Acc.toAddress.toString
    val miner3Addr           = miner3Acc.toAddress.toString

    step("Commit to generation")
    val period1 = node.currentGenerationPeriod.value.next

    val commitTxn1 = node.sign(CommitToGenerationRequest(sender = Some(miner1Addr)))
    commitTxn1.generationPeriodStart.value shouldBe period1.start.toInt

    val commitTxn2 = node.sign(CommitToGenerationRequest(sender = Some(miner2Addr)))
    commitTxn2.generationPeriodStart.value shouldBe period1.start.toInt

    node.broadcastRequest(commitTxn1)
    node.broadcastRequest(commitTxn2)
    node.waitForGenerationPeriod(period1)

    step("Generators")
    isolated {
      val generators = node.generators(period1.start)
      generators.size shouldBe 2
      generators shouldBe Seq(
        GeneratorsResponse.Entry(
          address = miner1Addr,
          balance = 9990598000000L,
          transactionId = commitTxn1.id
        ),
        GeneratorsResponse.Entry(
          address = miner2Addr,
          balance = 9989990000000L,
          transactionId = commitTxn2.id
        )
      )
    }

    step("Finalized height checks")
    val deadline               = 2.minutes.fromNow
    var finalizedHeight1       = node.finalizedHeight
    val waitingFinalizedHeight = finalizedHeight1 + 2

    var done = false
    while (!done && deadline.hasTimeLeft()) {
      val currHeight = node.height
      if (currHeight > waitingFinalizedHeight + 2)
        fail(
          s"Finalization height doesn't rise: height=$currHeight, waiting for finalized height=$waitingFinalizedHeight, last finalized height=$finalizedHeight1"
        )

      // We need at least one transaction, otherwise there won't be a microblock, thus no voting, no finalization
      node.transfer(miner1Acc, miner3Addr, 1.waves, waitForTx = true)

      val updatedFinalizedHeight = node.finalizedHeight
      if (updatedFinalizedHeight < finalizedHeight1)
        fail(s"Finalized height $updatedFinalizedHeight became lower than the previous $finalizedHeight1")
      else if (updatedFinalizedHeight != finalizedHeight1)
        log.debug(s"New finalized height: $finalizedHeight1 -> $updatedFinalizedHeight")

      finalizedHeight1 = updatedFinalizedHeight
      done = finalizedHeight1 >= waitingFinalizedHeight
    }

    node.waitForHeight(node.height + 1) // Finalization happened in a microblock

    step("Survives restart")
    isolated {
      val height = node.height
      docker.restartContainer(node)
      node.waitForHeight(height)
    }

    step("Finalized block header and height checks")
    val finalizedBlock1 = node.finalizedBlockHeader()
    finalizedBlock1.height should be >= finalizedHeight1

    val finalizedHeight2 = node.finalizedHeightAt(node.height)
    finalizedHeight2 should be >= finalizedHeight1

    val finalizedHeightBefore1 = node.finalizedHeightAt(finalizedBlock1.height)
    finalizedHeightBefore1 should be < finalizedHeight1

    step("Finalization voting in a block header")
    val blockHeader        = node.blockHeaderAt(node.height - 1)
    val finalizationVoting = blockHeader.finalizationVoting.value

    val generators: Seq[(data: GeneratorsResponse.Entry, index: Int)] = node.generators(blockHeader.height).zipWithIndex

    val minerEndorser = generators.find { g => g.data.address == blockHeader.generator }.value

    withClue(s"endorsers=[${finalizationVoting.endorserIndexes.mkString(", ")}], miner=${minerEndorser.index}: ") {
      finalizationVoting.endorserIndexes should not contain minerEndorser.index
    }

    val totalBalance = generators.map { g => BigInt(g.data.balance) }.sum
    val votedBalance = generators.collect {
      case g if finalizationVoting.endorserIndexes.contains(g.index) || g.index == minerEndorser.index => BigInt(g.data.balance)
    }.sum

    withClue(s"totalBalance=$totalBalance, votedBalance=$votedBalance: ") {
      votedBalance * 2 should be >= (totalBalance * 2)
    }

    step("Force rollback")
    val startHeight = waitingFinalizedHeight + 2
    node.waitForHeight(startHeight)

    val currentFinalizedHeight = node.finalizedHeight
    currentFinalizedHeight should be >= finalizedHeight1
    node.rollback(currentFinalizedHeight - 1, returnToUTX = false)
    node.waitFor("finalizedHeight decreased")(_.finalizedHeight, _ < currentFinalizedHeight, 1.second)
  }
}

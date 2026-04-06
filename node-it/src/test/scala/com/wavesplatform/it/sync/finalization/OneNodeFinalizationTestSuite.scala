package com.wavesplatform.it.sync.finalization

import com.typesafe.config.Config
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.BaseFreeSpec
import com.wavesplatform.it.api.*
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.state.Height
import com.wavesplatform.test.NumericExt
import com.wavesplatform.utils.ScorexLogging
import org.apache.pekko.http.scaladsl.model.StatusCodes
import org.scalatest.OptionValues

import scala.concurrent.duration.DurationInt

class OneNodeFinalizationTestSuite extends BaseFreeSpec, OptionValues, ScorexLogging {
  import com.wavesplatform.it.NodeConfigs.*
  override val nodeConfigs: Seq[Config] = Seq(
    BiggestMiner.quorum(0).preactivatedFeatures(
      BlockchainFeatures.FairPoS,
      BlockchainFeatures.DeterministicFinality
    )
  )

  private def node            = dockerNodes().last
  private lazy val miner1Acc  = node.keyPair
  private lazy val miner1Addr = node.address

  "finalization activated and works" in {
    val miner2Acc, miner3Acc = node.createKeyPairServerSide()
    val miner2Addr           = miner2Acc.toAddress.toString
    val miner3Addr           = miner3Acc.toAddress.toString

    log.warn(s"M2=$miner2Addr, M3=$miner3Addr")

    step("Commit to generation")
    val period1 = node.currentGenerationPeriod.value.next

    val commitTxn1 = node.signCommitToGenerationRequest(miner1Addr)
    commitTxn1.generationPeriodStart.value shouldBe period1.start.toInt

    val commitTxn2 = node.signCommitToGenerationRequest(miner2Addr)
    commitTxn2.generationPeriodStart.value shouldBe period1.start.toInt

    node.broadcastRequest(commitTxn1)
    node.broadcastRequest(commitTxn2)
    node.waitForGenerationPeriod(period1)

    step("Generators")
    isolated {
      val generators = node.generators(period1.start)
      generators.size shouldBe 2
      generators.map(e => e.address -> e.transactionId) should contain theSameElementsAs Seq(
        miner1Addr -> commitTxn1.id,
        miner2Addr -> commitTxn2.id
      )
    }

    step("Finalized height checks")
    val finalizedHeight1       = node.finalizedHeight
    val waitingFinalizedHeight = finalizedHeight1 + 2

    withClue("Finalized height is unknown: ") {
      try node.finalizedHeightAt(node.height)
      catch {
        case ApiCallException(e: UnexpectedStatusCodeException) => e.statusCode shouldBe StatusCodes.NotFound.intValue
      }

      try node.finalizedHeightAt(node.height + 10)
      catch {
        case ApiCallException(e: UnexpectedStatusCodeException) => e.statusCode shouldBe StatusCodes.NotFound.intValue
      }
    }

    // We need at least one transaction, otherwise there won't be a microblock, thus no voting, no finalization
    // Finalization happened in a microblock
    node.waitForHeight(Height(node.waitForTransaction(node.transfer(miner1Acc, miner3Addr, 1.waves, waitForTx = true).id).height + 1))
    val fs = node.finalityStatus
    if (fs.height > waitingFinalizedHeight + 2)
      fail(
        s"Finalization height doesn't rise: height=${fs.height}, waiting for finalized height=$waitingFinalizedHeight, last finalized height=$finalizedHeight1"
      )

    if (fs.finalizedHeight < finalizedHeight1)
      fail(s"Finalized height ${fs.finalizedHeight} became lower than the previous $finalizedHeight1")
    else if (fs.finalizedHeight != finalizedHeight1)
      log.debug(s"New finalized height: $finalizedHeight1 -> ${fs.finalizedHeight}")

    step("Survives restart")
    isolated {
      val height = node.height
      docker.restartContainer(node)
      node.waitForHeight(height)
    }

    step("Finalized block header and height checks")
    val finalizedBlock1 = node.finalizedBlockHeader()
    finalizedBlock1.height should be >= finalizedHeight1
    node.finalizedHeightAt(finalizedBlock1.height) should be <= finalizedBlock1.height

    step("Finalization voting in a block header")
    val votingBlockHeader  = node.blockHeaderAt(finalizedBlock1.height + 1)
    val finalizationVoting = votingBlockHeader.finalizationVoting.value

    val generators: Seq[(data: GeneratorsResponse.Entry, index: Int)] = node.generators(votingBlockHeader.height).zipWithIndex

    val minerEndorser = generators.find { g => g.data.address == votingBlockHeader.generator }.value

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

package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.api.http.requests.CommitToGenerationRequest
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.api.*
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.{BaseFreeSpec, NodeConfigs}
import com.wavesplatform.test.NumericExt
import org.scalatest.OptionValues

import scala.concurrent.duration.DurationInt

class FinalizationTestSuite extends BaseFreeSpec with OptionValues {
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.DeterministicFinality.id, 0)))
      .withDefault(1)
      .buildNonConflicting()

  private def node            = dockerNodes().last
  private lazy val miner1Acc  = node.keyPair
  private lazy val miner1Addr = node.address

  "finalization activated and works" in {
    val miner2Acc, miner3Acc = node.createKeyPairServerSide()
    val miner2Addr           = miner2Acc.toAddress.toString
    val miner3Addr           = miner3Acc.toAddress.toString

    val period1 = node.currentGenerationPeriod.next

    val commitTxn1 = node.sign(CommitToGenerationRequest(sender = Some(miner1Addr)))
    commitTxn1.generationPeriodStart.value shouldBe period1.start

    val commitTxn2 = node.sign(CommitToGenerationRequest(sender = Some(miner2Addr)))
    commitTxn2.generationPeriodStart.value shouldBe period1.start

    node.broadcastRequest(commitTxn1)
    node.broadcastRequest(commitTxn2)
    node.waitForGenerationPeriod(period1)

    val generators1 = node.generators(period1.start)
    generators1.size shouldBe 2
    generators1 shouldBe Seq(
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

    info("Finalized height checks")
    val deadline               = 2.minutes.fromNow
    val finalizedHeight1       = node.finalizedHeight
    val waitingFinalizedHeight = finalizedHeight1 + 2

    var currFinalizedHeight = finalizedHeight1
    var done                = false
    while (!done && deadline.hasTimeLeft()) {
      val currHeight = node.height
      if (currHeight > waitingFinalizedHeight + 2)
        fail(s"Finalization height doesn't rise: height=$currHeight, waiting for finalized height=$waitingFinalizedHeight")

      // We need at least one transaction, otherwise there won't be a microblock, thus no voting, no finalization
      node.transfer(miner1Acc, miner3Addr, 1.waves, waitForTx = true)

      val updatedFinalizedHeight = node.finalizedHeight
      if (updatedFinalizedHeight < currFinalizedHeight)
        fail(s"Finalized height $updatedFinalizedHeight became lower than the previous $currFinalizedHeight")
      else if (updatedFinalizedHeight != currFinalizedHeight)
        log.debug(s"New finalized height: $currFinalizedHeight -> $updatedFinalizedHeight")

      currFinalizedHeight = updatedFinalizedHeight
      done = currFinalizedHeight > waitingFinalizedHeight
    }

    info("Finalized header checks")
    val finalizedBlock = node.finalizedBlockHeader()
    finalizedBlock.height should be >= currFinalizedHeight
  }
}

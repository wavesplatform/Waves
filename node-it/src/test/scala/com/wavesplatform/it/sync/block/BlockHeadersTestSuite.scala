package com.wavesplatform.it.sync.block

import com.typesafe.config.Config
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{Node, NodeConfigs, TransferSending}
import org.scalatest._

import scala.concurrent.duration._
import scala.concurrent.Await

class BlockHeadersTestSuite extends FunSuite with CancelAfterFailure with TransferSending with NodesFromDocker with Matchers {

  private val activationHeight   = 4
  private val minerDesiredReward = 750000000
  private val minIncrement       = 50000000
  private val initialReward      = 600000000
  private val rewardTerm         = 3
  private val votingInterval     = 2

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.raw(
        s"""waves {
           |  blockchain.custom.functionality {
           |    pre-activated-features = {
           |      ${BlockchainFeatures.BlockReward.id} = $activationHeight
           |    }
           |  }
           |  blockchain.custom.rewards {
           |    term = $rewardTerm
           |    initial = $initialReward
           |    min-increment = $minIncrement
           |    voting-interval = $votingInterval
           |  }
           |  rewards.desired = $minerDesiredReward
           |  miner.quorum = 1
           |}""".stripMargin
      ))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

  private def notMiner: Node = nodes.last

  private val nodeAddresses = nodeConfigs.map(_.getString("address")).toSet

  def assertBlockInfo(blocks: Block, blockHeaders: BlockHeaders): Unit = {
    blockHeaders.generator shouldBe blocks.generator
    blockHeaders.timestamp shouldBe blocks.timestamp
    blockHeaders.signature shouldBe blocks.signature
    blockHeaders.desiredReward shouldBe blocks.desiredReward
    blockHeaders.reward shouldBe blocks.reward
    blockHeaders.transactionCount shouldBe blocks.transactions.size
  }

  test("blockAt content should be equal to blockHeaderAt, except transactions info") {
    val baseHeight = nodes.map(_.height).max
    Await.result(processRequests(generateTransfersToRandomAddresses(10, nodeAddresses)), 2.minutes)
    nodes.waitForHeight(baseHeight + 4)
    notMiner.blockHeadersAt(activationHeight).reward shouldBe Some(initialReward)
    notMiner.blockHeadersAt(activationHeight + 1).desiredReward shouldBe Some(minerDesiredReward)
    val blocks        = notMiner.blockAt(baseHeight + 1)
    val blocksHeaders = notMiner.blockHeadersAt(baseHeight + 1)

    assertBlockInfo(blocks, blocksHeaders)
    nodes.waitForHeight(activationHeight + rewardTerm)
//    println(notMiner.blockSeq(1, activationHeight + rewardTerm).map(_.desiredReward))

    notMiner.blockHeadersAt(activationHeight + rewardTerm).reward shouldBe Some(initialReward + minIncrement)


  }

  test("lastBlock content should be equal to lastBlockHeader, except transactions info") {
    val baseHeight = nodes.map(_.height).max
    Await.result(processRequests(generateTransfersToRandomAddresses(30, nodeAddresses)), 2.minutes)
    nodes.waitForHeight(baseHeight + 1)
    val blocks        = nodes.map(_.lastBlock)
    val blocksHeaders = nodes.map(_.lastBlockHeaders)
    blocks.zip(blocksHeaders).foreach { case (k, v) => assertBlockInfo(k, v) }
  }

  test("blockSeq content should be equal to blockHeaderSeq, except transactions info") {
    val baseHeight = nodes.map(_.height).max
    Await.result(processRequests(generateTransfersToRandomAddresses(30, nodeAddresses)), 2.minutes)
    nodes.waitForSameBlockHeadersAt(baseHeight + 3)
    val blocks       = nodes.head.blockSeq(baseHeight + 1, baseHeight + 3)
    val blockHeaders = nodes.head.blockHeadersSeq(baseHeight + 1, baseHeight + 3)

    blocks.zip(blockHeaders).foreach {
      case (block, header) =>
        header.generator shouldBe block.generator
        header.timestamp shouldBe block.timestamp
        header.signature shouldBe block.signature
        header.desiredReward shouldBe block.desiredReward
        header.reward shouldBe block.reward
        header.transactionCount shouldBe block.transactions.size
    }
  }

  test("blocks/address produces correct result") {
    val miner  = nodes.head
    val height = miner.height

    val minerBlocks    = miner.blockSeqByAddress(miner.address, 1, height)
    val nonMinerBlocks = notMiner.blockSeqByAddress(notMiner.address, 1, height)

    minerBlocks.size shouldEqual (height - 1)
    nonMinerBlocks shouldBe empty
  }
}

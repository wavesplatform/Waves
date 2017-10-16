package scorex.transaction

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.state2._
import com.wavesplatform.history._
import org.scalatest.{FunSuite, Matchers}
import scorex.block.Block

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class BlockchainUpdaterTest extends FunSuite with Matchers with HistoryTest {

  private val ApprovalPeriod = 100

  private val WavesSettings = DefaultWavesSettings.copy(blockchainSettings =
    DefaultWavesSettings.blockchainSettings.copy(
      functionalitySettings = DefaultWavesSettings.blockchainSettings.functionalitySettings.copy(
        featureCheckBlocksPeriod = ApprovalPeriod,
        blocksForFeatureActivation = (ApprovalPeriod * 0.9).toInt
      )
    )
  )

  private def storageFactory() = StorageFactory(WavesSettings, new ReentrantReadWriteLock(true)).get

  test("concurrent access to lastBlock doesn't throw any exception") {
    val (h, fp, _, _, bu, _) = storageFactory()

    bu.processBlock(genesisBlock)

    (1 to 1000).foreach { _ =>
      bu.processBlock(getNextTestBlock(h))
    }

    @volatile var failed = false

    (1 to 1000).foreach { _ =>
      Future(bu.processBlock(getNextTestBlock(h))).recover[Any] { case e => e.printStackTrace(); failed = true }
      Future(bu.removeAfter(h.lastBlockIds(2).last)).recover[Any] { case e => e.printStackTrace(); failed = true }
    }

    Thread.sleep(1000)

    failed shouldBe false
  }

  def appendBlock(block: Block, blockchainUpdater: BlockchainUpdater): Unit = {
    blockchainUpdater.processBlock(block)
  }

  test("features approved and accepted as height grows") {

    val (h, fp, _, _, bu, _) = storageFactory()

    bu.processBlock(genesisBlock)

    fp.featureStatus(1, 1) shouldBe BlockchainFeatureStatus.Undefined
    fp.featureStatus(2, 1) shouldBe BlockchainFeatureStatus.Undefined
    fp.featureStatus(3, 1) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(1)))
    }

    h.height() shouldBe ApprovalPeriod
    fp.featureStatus(1, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved
    fp.featureStatus(2, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined
    fp.featureStatus(3, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(2)))
    }

    h.height() shouldBe 2 * ApprovalPeriod
    fp.featureStatus(1, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    fp.featureStatus(2, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved
    fp.featureStatus(3, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set()))
    }

    h.height() shouldBe 3 * ApprovalPeriod
    fp.featureStatus(1, 3 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    fp.featureStatus(2, 3 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    fp.featureStatus(3, 3 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined
  }

  test("features rollback with block rollback") {
    val (h, fp, _, _, bu, _) = storageFactory()

    bu.processBlock(genesisBlock)

    fp.featureStatus(1, 1) shouldBe BlockchainFeatureStatus.Undefined
    fp.featureStatus(2, 1) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(1))).explicitGet()
    }

    h.height() shouldBe ApprovalPeriod
    fp.featureStatus(1, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved
    fp.featureStatus(2, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

    bu.removeAfter(h.lastBlockIds(2).last).explicitGet()

    h.height() shouldBe ApprovalPeriod - 1
    fp.featureStatus(1, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined
    fp.featureStatus(2, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod + 1).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(2))).explicitGet()
    }

    h.height() shouldBe 2 * ApprovalPeriod
    fp.featureStatus(1, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    fp.featureStatus(2, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved

    bu.removeAfter(h.lastBlockIds(2).last).explicitGet()

    h.height() shouldBe 2 * ApprovalPeriod - 1
    fp.featureStatus(1, 2 * ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Approved
    fp.featureStatus(2, 2 * ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined

    bu.processBlock(getNextTestBlockWithVotes(h, Set.empty)).explicitGet()

    h.height() shouldBe 2 * ApprovalPeriod
    fp.featureStatus(1, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    fp.featureStatus(2, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved

    bu.removeAfter(h.lastBlockIds(2).last).explicitGet()

    h.height() shouldBe 2 * ApprovalPeriod - 1
    fp.featureStatus(1, 2 * ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Approved
    fp.featureStatus(2, 2 * ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined

    bu.removeAfter(h.lastBlockIds(ApprovalPeriod + 1).last).explicitGet()

    h.height() shouldBe ApprovalPeriod - 1
    fp.featureStatus(1, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined
    fp.featureStatus(2, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined
  }

  test("feature activation height is not overrided with further periods") {
    val (h, fp, _, _, bu, _) = storageFactory()

    bu.processBlock(genesisBlock)

    fp.featureStatus(1, 1) shouldBe BlockchainFeatureStatus.Undefined

    fp.featureActivationHeight(1) shouldBe None

    (1 until ApprovalPeriod).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(1))).explicitGet()
    }

    fp.featureActivationHeight(1) shouldBe Some(ApprovalPeriod * 2)

    (1 to ApprovalPeriod).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(1))).explicitGet()
    }

    fp.featureActivationHeight(1) shouldBe Some(ApprovalPeriod * 2)
  }

  test("feature activated only by 90% of blocks") {
    val (h, fp, _, _, bu, _) = storageFactory()

    bu.processBlock(genesisBlock)

    fp.featureStatus(1, 1) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { i =>
      bu.processBlock(getNextTestBlockWithVotes(h, if (i % 2 == 0) Set(1) else Set())).explicitGet()
    }
    fp.featureStatus(1, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod).foreach { i =>
      bu.processBlock(getNextTestBlockWithVotes(h, if (i % 10 == 0) Set() else Set(1))).explicitGet()
    }
    fp.featureStatus(1, ApprovalPeriod * 2) shouldBe BlockchainFeatureStatus.Approved

    (1 to ApprovalPeriod).foreach { i =>
      bu.processBlock(getNextTestBlock(h)).explicitGet()
    }
    fp.featureStatus(1, ApprovalPeriod * 3) shouldBe BlockchainFeatureStatus.Activated
  }

  test("features votes resets when voting window changes") {
    val (h, fp, _, _, bu, _) = storageFactory()

    bu.processBlock(genesisBlock)

    fp.featureVotesCountWithinActivationWindow(h.height()) shouldBe Map.empty

    fp.featureStatus(1, h.height()) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { i =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(1)))
      fp.featureVotesCountWithinActivationWindow(h.height()) shouldBe Map(1.toShort -> i)
    }

    fp.featureStatus(1, h.height()) shouldBe BlockchainFeatureStatus.Approved

    bu.processBlock(getNextTestBlockWithVotes(h, Set(1)))
    fp.featureVotesCountWithinActivationWindow(h.height()) shouldBe Map(1.toShort -> 1)

    fp.featureStatus(1, h.height()) shouldBe BlockchainFeatureStatus.Approved
  }
}

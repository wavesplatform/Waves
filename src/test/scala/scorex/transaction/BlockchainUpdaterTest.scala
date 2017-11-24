package scorex.transaction

import java.security.Permission
import java.util.concurrent.{Semaphore, TimeUnit}

import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.history._
import com.wavesplatform.state2.diffs.produce
import org.scalatest.words.ShouldVerb
import com.wavesplatform.state2._
import org.scalatest.{FunSuite, Matchers}
import scorex.block.Block

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class BlockchainUpdaterTest extends FunSuite with Matchers with HistoryTest with ShouldVerb{

  private val ApprovalPeriod = 100

  private val WavesSettings = DefaultWavesSettings.copy(blockchainSettings =
    DefaultWavesSettings.blockchainSettings.copy(
      functionalitySettings = DefaultWavesSettings.blockchainSettings.functionalitySettings.copy(
        featureCheckBlocksPeriod = ApprovalPeriod,
        blocksForFeatureActivation = (ApprovalPeriod * 0.9).toInt
      )
    ),
    featuresSettings = DefaultWavesSettings.featuresSettings.copy(autoShutdownOnUnsupportedFeature = true)
  )

  private def storageFactory() = StorageFactory(open(), WavesSettings).get

  ignore ("concurrent access to lastBlock doesn't throw any exception") {
    val (h, fp, _, bu, _) = storageFactory()

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

    val (h, fp, _, bu, _) = storageFactory()

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
    val (h, fp, _, bu, _) = storageFactory()

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
    val (h, fp, _, bu, _) = storageFactory()

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
    val (h, fp, _, bu, _) = storageFactory()

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
    val (h, fp, _, bu, _) = storageFactory()

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

  test("block processing should fail if unimplemented feature was activated on blockchaing when autoShutdownOnUnsupportedFeature = yes and exit with code 38") {

    val signal = new Semaphore(1)
    signal.acquire()

    System.setSecurityManager(new SecurityManager {
      override def checkPermission(perm: Permission): Unit = {}

      override def checkPermission(perm: Permission, context: Object): Unit = {}

      override def checkExit(status: Int): Unit = signal.synchronized {
        super.checkExit(status)
        if(status == 38)
          signal.release()
        throw new SecurityException("System exit not allowed")
      }
    })


    val (h, fp, _, bu, _) = StorageFactory(open(), WavesSettings).get
    bu.processBlock(genesisBlock)

    (1 to ApprovalPeriod * 2).foreach { i =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(-1))).explicitGet()
    }

    bu.processBlock(getNextTestBlockWithVotes(h, Set(-1))) should produce("ACTIVATED ON BLOCKCHAIN")

    signal.tryAcquire(10, TimeUnit.SECONDS)

    System.setSecurityManager(null)
  }

  test("sunny day test when known feature activated") {
    val (h, fp, _, bu, _) = StorageFactory(open(), WavesSettings).get
    bu.processBlock(genesisBlock)

    (1 until ApprovalPeriod * 2 - 1).foreach { i =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(1))).explicitGet()
    }

    fp.featureStatus(1, h.height()) should be(BlockchainFeatureStatus.Approved)
    bu.processBlock(getNextTestBlockWithVotes(h, Set(1))).explicitGet()
    fp.featureStatus(1, h.height()) should be(BlockchainFeatureStatus.Activated)
  }
}

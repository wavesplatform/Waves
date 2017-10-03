package scorex.transaction

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.features.{BlockchainFeatureStatus, FeatureProvider}
import com.wavesplatform.history.HistoryWriterImpl
import com.wavesplatform.settings.FeaturesSettings
import com.wavesplatform.state2._
import com.wavesplatform.history._
import com.wavesplatform.state2.reader.StateReader
import org.scalacheck.Shrink
import org.scalatest.{FunSuite, Matchers}
import scorex.block.Block
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class HistoryWriterTest extends FunSuite with Matchers with HistoryTest {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  private val ApprovalPeriod = 10000

  private val FeaturesSettingsWithoutSupportedFeatures: FeaturesSettings =
    FeaturesSettings(autoShutdownOnUnsupportedFeature = false, List.empty)

  /*
  test("concurrent access to lastBlock doesn't throw any exception") {
    val history = HistoryWriterImpl(None, new ReentrantReadWriteLock(), TestFunctionalitySettings.Enabled,
      TestFunctionalitySettings.EmptyFeaturesSettings).get
    appendGenesisBlock(history)

    (1 to 1000).foreach { _ =>
      appendTestBlock(history)
    }

    @volatile var failed = false

    def tryAppendTestBlock(history: HistoryWriterImpl): Either[ValidationError, BlockDiff] =
      history.appendBlock(TestBlock.withReference(history.lastBlock.get.uniqueId), )(Right(BlockDiff.empty))

    (1 to 1000).foreach { _ =>
      Future(tryAppendTestBlock(history)).recover[Any] { case e => e.printStackTrace(); failed = true }
      Future(history.discardBlock()).recover[Any] { case e => e.printStackTrace(); failed = true }
    }
    Thread.sleep(1000)

    failed shouldBe false
  }
  */

  def appendBlock(block: Block, blockchainUpdater: BlockchainUpdater): Unit = {
    blockchainUpdater.processBlock(block)
  }

  test("features approved and accepted as height grows") {

    val (h, fp, _, _, bu, _) = StorageFactory(RootDefaultSettings, EmptyFeaturesSettings).get

    bu.processBlock(genesisBlock)

    fp.featureStatus(1, 1) shouldBe BlockchainFeatureStatus.Undefined
    fp.featureStatus(2, 1) shouldBe BlockchainFeatureStatus.Undefined
    fp.featureStatus(3, 1) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(1)))
    }

    h.height() shouldBe ApprovalPeriod
    fp.featureStatus(1, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Accepted
    fp.featureStatus(2, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined
    fp.featureStatus(3, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(2)))
    }

    h.height() shouldBe 2 * ApprovalPeriod
    fp.featureStatus(1, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    fp.featureStatus(2, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Accepted
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
    val (h, fp, _, _, bu, _) = StorageFactory(RootDefaultSettings, EmptyFeaturesSettings).get

    bu.processBlock(genesisBlock)

    fp.featureStatus(1, 1) shouldBe BlockchainFeatureStatus.Undefined
    fp.featureStatus(2, 1) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(1)))
    }

    h.height() shouldBe ApprovalPeriod
    fp.featureStatus(1, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Accepted
    fp.featureStatus(2, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

    bu.removeAfter(h.lastBlockIds(2).last).explicitGet()

    h.height() shouldBe ApprovalPeriod - 1
    fp.featureStatus(1, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined
    fp.featureStatus(2, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod + 1).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(2)))
    }

    h.height() shouldBe 2 * ApprovalPeriod
    fp.featureStatus(1, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    fp.featureStatus(2, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Accepted

    bu.removeAfter(h.lastBlockIds(2).last).explicitGet()

    h.height() shouldBe 2 * ApprovalPeriod - 1
    fp.featureStatus(1, 2 * ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Accepted
    fp.featureStatus(2, 2 * ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined

    bu.removeAfter(h.lastBlockIds(2).last).explicitGet()
    bu.removeAfter(h.lastBlockIds(2).last).explicitGet()
    bu.removeAfter(h.lastBlockIds(2).last).explicitGet()
    bu.removeAfter(h.lastBlockIds(2).last).explicitGet()

    h.height() shouldBe ApprovalPeriod - 1
    fp.featureStatus(1, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined
    fp.featureStatus(2, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined
  }

  /*
  test("feature activation height") {
    val history = HistoryWriterImpl(None, new ReentrantReadWriteLock(), TestFunctionalitySettings.Enabled,
      FeaturesSettingsWithoutSupportedFeatures).get

    appendGenesisBlock(history)
    history.featureStatus(1, 1) shouldBe BlockchainFeatureStatus.Undefined

    history.featureActivationHeight(1) shouldBe None

    (1 until ApprovalPeriod).foreach { _ =>
      appendTestBlock3(history, Set(1))
    }

    history.featureActivationHeight(1) shouldBe Some(ApprovalPeriod * 2)

    (1 to ApprovalPeriod).foreach { _ =>
      appendTestBlock3(history, Set(1))
    }

    history.featureActivationHeight(1) shouldBe Some(ApprovalPeriod * 2)
  }

  test("feature activated only by 90% of blocks") {
    val history = HistoryWriterImpl(None, new ReentrantReadWriteLock(), TestFunctionalitySettings.Enabled,
      FeaturesSettingsWithoutSupportedFeatures).get

    appendGenesisBlock(history)
    history.featureStatus(1, 1) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { i =>
      appendTestBlock3(history, if (i % 2 == 0) Set(1) else Set())
    }
    history.featureStatus(1, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod).foreach { i =>
      appendTestBlock3(history, if (i % 10 == 0) Set() else Set(1))
    }
    history.featureStatus(1, ApprovalPeriod * 2) shouldBe BlockchainFeatureStatus.Accepted

    (1 to ApprovalPeriod).foreach { i =>
      appendTestBlock3(history, Set())
    }
    history.featureStatus(1, ApprovalPeriod * 3) shouldBe BlockchainFeatureStatus.Activated
  }
  */
}

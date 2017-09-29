package scorex.transaction

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.history.HistoryWriterImpl
import com.wavesplatform.settings.FeaturesSettings
import com.wavesplatform.state2._
import org.scalatest.{FunSuite, Matchers}
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class HistoryWriterTest extends FunSuite with Matchers with HistoryTest {

  private val ApprovalPeriod = 10000

  private val FeaturesSettingsWithoutSupportedFeatures: FeaturesSettings =
    FeaturesSettings(autoShutdownOnUnsupportedFeature = false, List.empty)

  test("concurrent access to lastBlock doesn't throw any exception") {
    val history = HistoryWriterImpl(None, new ReentrantReadWriteLock(), TestFunctionalitySettings.Enabled,
      TestFunctionalitySettings.EmptyFeaturesSettings).get
    appendGenesisBlock(history)

    (1 to 1000).foreach { _ =>
      appendTestBlock(history)
    }

    @volatile var failed = false

    def tryAppendTestBlock(history: HistoryWriterImpl): Either[ValidationError, BlockDiff] =
      history.appendBlock(TestBlock.withReference(history.lastBlock.get.uniqueId))(Right(BlockDiff.empty))

    (1 to 1000).foreach { _ =>
      Future(tryAppendTestBlock(history)).recover[Any] { case e => e.printStackTrace(); failed = true }
      Future(history.discardBlock()).recover[Any] { case e => e.printStackTrace(); failed = true }
    }
    Thread.sleep(1000)

    failed shouldBe false
  }

  test("features approved and accepted as height grows") {
    val history = HistoryWriterImpl(None, new ReentrantReadWriteLock(), TestFunctionalitySettings.Enabled,
      FeaturesSettingsWithoutSupportedFeatures).get

    appendGenesisBlock(history)

    history.featureStatus(1) shouldBe BlockchainFeatureStatus.Undefined
    history.featureStatus(2) shouldBe BlockchainFeatureStatus.Undefined
    history.featureStatus(3) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { _ =>
      appendTestBlock3(history, Set(1))
    }

    history.height() shouldBe ApprovalPeriod
    history.featureStatus(1) shouldBe BlockchainFeatureStatus.Accepted
    history.featureStatus(2) shouldBe BlockchainFeatureStatus.Undefined
    history.featureStatus(3) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod).foreach { _ =>
      appendTestBlock3(history, Set(2))
    }

    history.height() shouldBe 2 * ApprovalPeriod
    history.featureStatus(1) shouldBe BlockchainFeatureStatus.Activated
    history.featureStatus(2) shouldBe BlockchainFeatureStatus.Accepted
    history.featureStatus(3) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod).foreach { _ =>
      appendTestBlock3(history, Set())
    }

    history.height() shouldBe 3 * ApprovalPeriod
    history.featureStatus(1) shouldBe BlockchainFeatureStatus.Activated
    history.featureStatus(2) shouldBe BlockchainFeatureStatus.Activated
    history.featureStatus(3) shouldBe BlockchainFeatureStatus.Undefined
  }

  test("last block should affect feature voting the same way either liquid or saved to history") {
    val history = HistoryWriterImpl(None, new ReentrantReadWriteLock(), TestFunctionalitySettings.Enabled,
      FeaturesSettingsWithoutSupportedFeatures).get

    appendGenesisBlock(history)

    history.featureStatus(1) shouldBe BlockchainFeatureStatus.Undefined
    history.featureStatus(2) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod - 1).foreach { _ =>
      appendTestBlock3(history, Set(1))
    }

    history.height() shouldBe ApprovalPeriod - 1

    //one block before feature check
    history.featureStatus(1) shouldBe BlockchainFeatureStatus.Undefined
    history.featureStatus(2) shouldBe BlockchainFeatureStatus.Undefined

    val nextBlock = getNextTestBlock(history)

    //last ng block
//    history.featureStatus(1, Option(NgState(nextBlock, BlockDiff.empty, 0L))) shouldBe BlockchainFeatureStatus.Accepted
//    history.featureStatus(2, Option(NgState(nextBlock, BlockDiff.empty, 0L))) shouldBe BlockchainFeatureStatus.Undefined

    //last solid block
    history.appendBlock(nextBlock)(Right(BlockDiff.empty))
    history.featureStatus(1) shouldBe BlockchainFeatureStatus.Accepted
    history.featureStatus(2) shouldBe BlockchainFeatureStatus.Undefined
  }

    test("features rollback with block rollback") {
    val history = HistoryWriterImpl(None, new ReentrantReadWriteLock(), TestFunctionalitySettings.Enabled,
      FeaturesSettingsWithoutSupportedFeatures).get

    appendGenesisBlock(history)

    history.featureStatus(1) shouldBe BlockchainFeatureStatus.Undefined
    history.featureStatus(2) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { _ =>
      appendTestBlock3(history, Set(1))
    }

    history.height() shouldBe ApprovalPeriod
    history.featureStatus(1) shouldBe BlockchainFeatureStatus.Accepted
    history.featureStatus(2) shouldBe BlockchainFeatureStatus.Undefined

    history.discardBlock()

    history.height() shouldBe ApprovalPeriod - 1
    history.featureStatus(1) shouldBe BlockchainFeatureStatus.Undefined
    history.featureStatus(2) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod + 1).foreach { _ =>
      appendTestBlock3(history, Set(2))
    }

    history.height() shouldBe 2 * ApprovalPeriod
    history.featureStatus(1) shouldBe BlockchainFeatureStatus.Activated
    history.featureStatus(2) shouldBe BlockchainFeatureStatus.Accepted

    history.discardBlock()

    history.height() shouldBe 2 * ApprovalPeriod - 1
    history.featureStatus(1) shouldBe BlockchainFeatureStatus.Accepted
    history.featureStatus(2) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod).foreach { _ => history.discardBlock() }

    history.height() shouldBe ApprovalPeriod - 1
    history.featureStatus(1) shouldBe BlockchainFeatureStatus.Undefined
    history.featureStatus(2) shouldBe BlockchainFeatureStatus.Undefined
  }

  test("feature activation height") {
    val history = HistoryWriterImpl(None, new ReentrantReadWriteLock(), TestFunctionalitySettings.Enabled,
      FeaturesSettingsWithoutSupportedFeatures).get

    appendGenesisBlock(history)
    history.featureStatus(1) shouldBe BlockchainFeatureStatus.Undefined

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
    history.featureStatus(1) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { i =>
      appendTestBlock3(history, if (i % 2 == 0) Set(1) else Set())
    }
    history.featureStatus(1) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod).foreach { i =>
      appendTestBlock3(history, if (i % 10 == 0) Set() else Set(1))
    }
    history.featureStatus(1) shouldBe BlockchainFeatureStatus.Accepted

    (1 to ApprovalPeriod).foreach { i =>
      appendTestBlock3(history, Set())
    }
    history.featureStatus(1) shouldBe BlockchainFeatureStatus.Activated
  }
}

package scorex.transaction

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.features.{FeatureAccepted, FeatureActivated, FeatureDefined}
import com.wavesplatform.history.HistoryWriterImpl
import com.wavesplatform.state2._
import org.scalatest.{FunSuite, Matchers}
import scorex.lagonaki.mocks.TestBlock

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class HistoryWriterTest extends FunSuite with Matchers with HistoryTest {

  private val ApprovalPeriod = 10000

  test("concurrent access to lastBlock doesn't throw any exception") {
    val history = HistoryWriterImpl(None, new ReentrantReadWriteLock()).get
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
    val history = HistoryWriterImpl(None, new ReentrantReadWriteLock()).get

    appendGenesisBlock(history)

    history.status(1) shouldBe FeatureDefined
    history.status(2) shouldBe FeatureDefined
    history.status(3) shouldBe FeatureDefined

    (1 to ApprovalPeriod - 1).foreach { _ =>
      appendTestBlock3(history, Set(1))
    }

    history.height() shouldBe ApprovalPeriod
    history.status(1) shouldBe FeatureAccepted
    history.status(2) shouldBe FeatureDefined
    history.status(3) shouldBe FeatureDefined

    (1 to ApprovalPeriod).foreach { _ =>
      appendTestBlock3(history, Set(2))
    }

    history.height() shouldBe 2 * ApprovalPeriod
    history.status(1) shouldBe FeatureActivated
    history.status(2) shouldBe FeatureAccepted
    history.status(3) shouldBe FeatureDefined

    (1 to ApprovalPeriod).foreach { _ =>
      appendTestBlock3(history, Set())
    }

    history.height() shouldBe 3 * ApprovalPeriod
    history.status(1) shouldBe FeatureActivated
    history.status(2) shouldBe FeatureActivated
    history.status(3) shouldBe FeatureDefined
  }

  test("features rollback with block rollback") {
    val history = HistoryWriterImpl(None, new ReentrantReadWriteLock()).get

    appendGenesisBlock(history)

    history.status(1) shouldBe FeatureDefined
    history.status(2) shouldBe FeatureDefined

    (1 to ApprovalPeriod - 1).foreach { _ =>
      appendTestBlock3(history, Set(1))
    }

    history.height() shouldBe ApprovalPeriod
    history.status(1) shouldBe FeatureAccepted
    history.status(2) shouldBe FeatureDefined

    history.discardBlock()

    history.height() shouldBe ApprovalPeriod - 1
    history.status(1) shouldBe FeatureDefined
    history.status(2) shouldBe FeatureDefined

    (1 to ApprovalPeriod + 1).foreach { _ =>
      appendTestBlock3(history, Set(2))
    }

    history.height() shouldBe 2 * ApprovalPeriod
    history.status(1) shouldBe FeatureActivated
    history.status(2) shouldBe FeatureAccepted

    history.discardBlock()

    history.height() shouldBe 2 * ApprovalPeriod - 1
    history.status(1) shouldBe FeatureAccepted
    history.status(2) shouldBe FeatureDefined

    (1 to ApprovalPeriod).foreach { _ => history.discardBlock() }

    history.height() shouldBe ApprovalPeriod - 1
    history.status(1) shouldBe FeatureDefined
    history.status(2) shouldBe FeatureDefined
  }

  test("feature activated only by 90% of blocks") {
    val history = HistoryWriterImpl(None, new ReentrantReadWriteLock()).get

    appendGenesisBlock(history)
    history.status(1) shouldBe FeatureDefined

    (1 to ApprovalPeriod - 1).foreach { i =>
      appendTestBlock3(history, if (i % 2 == 0) Set(1) else Set())
    }
    history.status(1) shouldBe FeatureDefined

    (1 to ApprovalPeriod).foreach { i =>
      appendTestBlock3(history, if (i % 10 == 0) Set() else Set(1))
    }
    history.status(1) shouldBe FeatureAccepted

    (1 to ApprovalPeriod).foreach { i =>
      appendTestBlock3(history, Set())
    }
    history.status(1) shouldBe FeatureActivated
  }
}

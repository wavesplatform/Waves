package scorex.transaction

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.features.FeatureStatus
import com.wavesplatform.history.HistoryWriterImpl
import com.wavesplatform.state2._
import org.scalatest.{FunSuite, Matchers}
import scorex.lagonaki.mocks.TestBlock

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class HistoryWriterTest extends FunSuite with Matchers with HistoryTest {
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

    history.status(1) shouldBe FeatureStatus.Defined
    history.status(2) shouldBe FeatureStatus.Defined
    history.status(3) shouldBe FeatureStatus.Defined

    (1 to 9999).foreach { _ =>
      appendTestBlock3(history, Set(1))
    }

    history.height() shouldBe 10000
    history.status(1) shouldBe FeatureStatus.Accepted
    history.status(2) shouldBe FeatureStatus.Defined
    history.status(3) shouldBe FeatureStatus.Defined

    (1 to 10000).foreach { _ =>
      appendTestBlock3(history, Set(2))
    }

    history.height() shouldBe 20000
    history.status(1) shouldBe FeatureStatus.Activated
    history.status(2) shouldBe FeatureStatus.Accepted
    history.status(3) shouldBe FeatureStatus.Defined

    (1 to 10000).foreach { _ =>
      appendTestBlock3(history, Set())
    }

    history.height() shouldBe 30000
    history.status(1) shouldBe FeatureStatus.Activated
    history.status(2) shouldBe FeatureStatus.Activated
    history.status(3) shouldBe FeatureStatus.Defined
  }

  test("features rollback with block rollback") {
    val history = HistoryWriterImpl(None, new ReentrantReadWriteLock()).get

    appendGenesisBlock(history)

    history.status(1) shouldBe FeatureStatus.Defined
    history.status(2) shouldBe FeatureStatus.Defined

    (1 to 9999).foreach { _ =>
      appendTestBlock3(history, Set(1))
    }

    history.height() shouldBe 10000
    history.status(1) shouldBe FeatureStatus.Accepted
    history.status(2) shouldBe FeatureStatus.Defined

    history.discardBlock()

    history.height() shouldBe 9999
    history.status(1) shouldBe FeatureStatus.Defined
    history.status(2) shouldBe FeatureStatus.Defined

    (1 to 10001).foreach { _ =>
      appendTestBlock3(history, Set(2))
    }

    history.height() shouldBe 20000
    history.status(1) shouldBe FeatureStatus.Activated
    history.status(2) shouldBe FeatureStatus.Accepted

    history.discardBlock()

    history.height() shouldBe 19999
    history.status(1) shouldBe FeatureStatus.Accepted
    history.status(2) shouldBe FeatureStatus.Defined

    (1 to 10000).foreach { _ => history.discardBlock() }

    history.height() shouldBe 9999
    history.status(1) shouldBe FeatureStatus.Defined
    history.status(2) shouldBe FeatureStatus.Defined
  }

  test("feature activated only by 90% of blocks") {
    val history = HistoryWriterImpl(None, new ReentrantReadWriteLock()).get

    appendGenesisBlock(history)
    history.status(1) shouldBe FeatureStatus.Defined

    (1 to 9999).foreach { i =>
      appendTestBlock3(history, if (i % 2 == 0) Set(1) else Set())
    }
    history.status(1) shouldBe FeatureStatus.Defined

    (1 to 10000).foreach { i =>
      appendTestBlock3(history, if (i % 10 == 0) Set() else Set(1))
    }
    history.status(1) shouldBe FeatureStatus.Accepted

    (1 to 10000).foreach { i =>
      appendTestBlock3(history, Set())
    }
    history.status(1) shouldBe FeatureStatus.Activated
  }
}

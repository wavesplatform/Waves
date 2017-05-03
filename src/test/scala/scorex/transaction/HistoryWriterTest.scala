package scorex.transaction

import com.wavesplatform.history.HistoryWriterImpl
import org.h2.mvstore.MVStore
import org.scalatest.{FunSuite, Matchers}
import scorex.lagonaki.mocks.TestBlock
import com.wavesplatform.state2._
import scorex.transaction.TransactionParser.SignatureLength

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class HistoryWriterTest extends FunSuite with Matchers {

  test("concurrent access to lastBlock doesn't fail") {
    val history = new HistoryWriterImpl(new MVStore.Builder().open())

    def appendBlock(): Unit = history.appendBlock(TestBlock.withReference(history.lastBlock.uniqueId)).explicitGet()
    history.appendBlock(TestBlock.withReference(Array.fill(SignatureLength)(0: Byte))).explicitGet()
    Range(1, 1000).foreach { _ =>
      appendBlock()
    }

    @volatile var failed = false

    Range(1, 1000).foreach { _ =>
      Future(appendBlock()).recover { case e => e.printStackTrace(); failed = true }
      Future(history.discardBlock()).recover { case e => e.printStackTrace(); failed = true }
    }
    Thread.sleep(1000)

    failed shouldBe false
  }
}

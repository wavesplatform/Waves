package scorex.transaction

import com.wavesplatform.history.HistoryWriterImpl
import org.h2.mvstore.MVStore
import org.scalatest.{FunSuite, Matchers}
import scorex.lagonaki.mocks.TestBlock
import com.wavesplatform.state2._
import scorex.transaction.TransactionParser.SignatureLength

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class HistoryWriterTest extends FunSuite with Matchers with HistoryTest {

  test("concurrent access to lastBlock doesn't fail") {
    val history = new HistoryWriterImpl(new MVStore.Builder().open())
    appendGenesisBlock(history)

    (1 to 1000).foreach { _ =>
      appendTestBlock(history)
    }

    @volatile var failed = false

    (1 to 1000).foreach { _ =>
      Future(appendTestBlock(history)).recover { case e => e.printStackTrace(); failed = true }
      Future(history.discardBlock()).recover { case e => e.printStackTrace(); failed = true }
    }
    Thread.sleep(1000)

    failed shouldBe false
  }
}

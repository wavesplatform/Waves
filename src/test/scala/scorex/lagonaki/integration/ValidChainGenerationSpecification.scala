package scorex.lagonaki.integration

import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}
import scorex.lagonaki.{TestingCommons, TransactionTestingCommons}
import scorex.lagonaki.server.LagonakiApplication
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import scorex.utils.{ScorexLogging, untilTimeout}

import scala.concurrent.duration._

class ValidChainGenerationSpecification extends FunSuite with Matchers with BeforeAndAfterAll with ScorexLogging
with TransactionTestingCommons {

  import TestingCommons._

  def waitGenerationOfBlocks(howMany: Int): Unit = {
    val height = applications.head.blockStorage.history.height()
    untilTimeout(5.minutes, 10.seconds) {
      applications.foreach(_.blockStorage.history.height() should be >= height + howMany)
    }
  }

  test("generate 3 blocks and synchronize") {
    waitGenerationOfBlocks(3)
    val last = applications.head.blockStorage.history.lastBlock
    untilTimeout(5.minutes, 10.seconds) {
      applications.head.blockStorage.history.contains(last) shouldBe true
    }
  }

  test("Include valid transaction in new block") {
    val tx = genValidTransaction()
    UnconfirmedTransactionsDatabaseImpl.all().size shouldBe 1
    waitGenerationOfBlocks(2)
    applications.foreach(_.blockStorage.state.included(tx).isDefined shouldBe true)
  }

}
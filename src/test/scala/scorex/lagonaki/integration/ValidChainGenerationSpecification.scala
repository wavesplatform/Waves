package scorex.lagonaki.integration

import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}
import scorex.lagonaki.{TestingCommons, TransactionTestingCommons}
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import scorex.utils.{ScorexLogging, untilTimeout}

import scala.concurrent.duration._

class ValidChainGenerationSpecification extends FunSuite with Matchers with BeforeAndAfterAll with ScorexLogging
with TransactionTestingCommons {

  import TestingCommons._

  val peers = applications.tail
  val app = peers.head

  def waitGenerationOfBlocks(howMany: Int): Unit = {
    val height = peers.map(_.blockStorage.history.height()).max
    untilTimeout(5.minutes, 10.seconds) {
      peers.foreach(_.blockStorage.history.height() should be >= height + howMany)
    }
  }

  test("Include valid transaction in new block") {
    val tx = genValidTransaction()
    UnconfirmedTransactionsDatabaseImpl.all().size shouldBe 1
    waitGenerationOfBlocks(2)
    peers.foreach(_.blockStorage.state.included(tx).isDefined shouldBe true)
  }

  test("generate 3 blocks and synchronize") {
    val genBal = peers.flatMap(a => a.wallet.privateKeyAccounts()).map(app.blockStorage.state.generationBalance(_)).sum
    genBal should be >= (peers.head.transactionModule.InitialBalance / 2)
    waitGenerationOfBlocks(3)
    val last = peers.head.blockStorage.history.lastBlock
    untilTimeout(5.minutes, 10.seconds) {
      peers.head.blockStorage.history.contains(last) shouldBe true
    }
  }

}
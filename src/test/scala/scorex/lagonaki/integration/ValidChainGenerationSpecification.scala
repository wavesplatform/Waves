package scorex.lagonaki.integration

import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}
import scorex.lagonaki.TransactionTestingCommons
import scorex.lagonaki.server.LagonakiApplication
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import scorex.utils.{ScorexLogging, untilTimeout}

import scala.concurrent.duration._

class ValidChainGenerationSpecification extends FunSuite with Matchers with BeforeAndAfterAll with ScorexLogging
with TransactionTestingCommons {


  val applications = List(new LagonakiApplication("settings-local1.json"),
    new LagonakiApplication("settings-local2.json"))

  override protected def beforeAll(): Unit = {
    applications.head.run()
    Thread.sleep(5000)
    applications(1).run()
    applications.foreach(_.wallet.generateNewAccounts(10))
    applications.foreach(_.wallet.privateKeyAccounts().nonEmpty shouldBe true)
    applications.foreach(_.blockStorage.history.height() should be > 0)
    log.info("ValidChainGenerationSpecification initialized")
  }

  override protected def afterAll(): Unit = {
    applications.foreach(_.stopAll())
  }


  test("generate 3 blocks and syncronize") {
    val height = applications.head.blockStorage.history.height()
    untilTimeout(5.minutes, 10.seconds) {
      applications.foreach(_.blockStorage.history.height() should be >= height + 3)
    }
    val last = applications.head.blockStorage.history.lastBlock
    untilTimeout(5.minutes, 10.seconds) {
      applications.head.blockStorage.history.contains(last) shouldBe true
    }
  }

  test("Include valid transaction in new block") {
    val tx = genValidTransaction()
    val height = applications.map(_.blockStorage.history.height()).max
    UnconfirmedTransactionsDatabaseImpl.all().size shouldBe 1
    untilTimeout(5.minutes, 1.second) {
      applications.foreach(_.blockStorage.history.height() should be >= height + 1)
    }
    applications.foreach(_.blockStorage.state.included(tx).isDefined shouldBe true)
  }

}
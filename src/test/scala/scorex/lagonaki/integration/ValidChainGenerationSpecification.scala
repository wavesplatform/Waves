package scorex.lagonaki.integration

import dispatch._
import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.encode.Base58
import scorex.lagonaki.TransactionTestingCommons
import scorex.lagonaki.server.LagonakiApplication
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import scorex.utils.{ScorexLogging, untilTimeout}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

class ValidChainGenerationSpecification extends FunSuite with Matchers with BeforeAndAfterAll with ScorexLogging
with TransactionTestingCommons {


  val applications = List(new LagonakiApplication("settings-local1.json"),
    new LagonakiApplication("settings-local2.json"))

  def peer(a: LagonakiApplication): String = a.settings.bindAddress + ":" + a.settings.rpcPort

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

  test("Scorex API route") {
    val json: JsObject = getRequest("/scorex/status")
    (json \ "block generator status").as[String] shouldBe "generating"
    (json \ "history synchronization status").as[String] shouldBe "synced"

    (getRequest("/scorex/version") \ "version").as[String] should (startWith("Scorex") and include("v."))
  }

  test("Seed API route") {
    val length = Random.nextInt(4096)
    Base58.decode((getRequest("/seed/") \ "seed").as[String]).isSuccess shouldBe true
    Base58.decode((getRequest(s"/seed/$length") \ "seed").as[String]).get.size shouldBe length
  }

  test("Wallet API route") {
    applications.foreach{ a =>
      (getRequest("/wallet/", peer(a))  \ "exists").as[Boolean] shouldBe true
      (getRequest("/wallet/seed", peer(a))  \ "seed").as[String] shouldBe Base58.encode(a.settings.walletSeed.get)
    }
  }

  def getRequest(us: String, peer: String = "http://127.0.0.1:9085"): JsObject = {
    val request = Http(url(peer + us).GET)
    val response = Await.result(request, 10 seconds)
    response
    Json.parse(response.getResponseBody).as[JsObject]
  }
}
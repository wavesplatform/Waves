package scorex.lagonaki.integration

import dispatch._
import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}
import play.api.libs.json.{JsValue, Json}
import scorex.crypto.encode.Base58
import scorex.lagonaki.TransactionTestingCommons

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

class APISpecification extends FunSuite with Matchers with BeforeAndAfterAll with TransactionTestingCommons {

  import scorex.lagonaki.TestingCommons._

  test("Seed API route") {
    val length = Random.nextInt(4096)
    Base58.decode((getRequest("/seed/") \ "seed").as[String]).isSuccess shouldBe true
    Base58.decode((getRequest(s"/seed/$length") \ "seed").as[String]).get.length shouldBe length
  }

  test("Wallet API route") {
    (getRequest("/wallet/") \ "exists").as[Boolean] shouldBe true
    (getRequest("/wallet/seed") \ "seed").as[String] shouldBe Base58.encode(application.settings.walletSeed.get)
  }


  test("Peers API route") {
    val connected = getRequest("/peers/connected")
    (connected \\ "declaredAddress").toList.size should be >= 1
    (connected \\ "peerName").toList.size should be >= 1
    (connected \\ "peerNonce").toList.size should be >= 1

    val all = getRequest("/peers/all")
    (all \\ "address").toList.size should be >= 1
    (all \\ "nodeName").toList.size should be >= 1
    (all \\ "nodeNonce").toList.size should be >= 1

    val blacklisted = getRequest("/peers/blacklisted")
    blacklisted.toString() shouldBe "[]"

    //TODO peers/connect
  }


  def getRequest(us: String, peer: String = peerUrl(application)): JsValue = {
    val request = Http(url(peer + us).GET)
    val response = Await.result(request, 10.seconds)
    Json.parse(response.getResponseBody)
  }
}
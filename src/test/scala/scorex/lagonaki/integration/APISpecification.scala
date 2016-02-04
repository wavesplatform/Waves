package scorex.lagonaki.integration

import dispatch._
import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.encode.Base58
import scorex.lagonaki.TransactionTestingCommons

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

class APISpecification extends FunSuite with Matchers with BeforeAndAfterAll with TransactionTestingCommons {

  import scorex.lagonaki.TestingCommons._

  test("Scorex API route") {
    val json: JsObject = getRequest("/scorex/status")
    (json \ "block generator status").as[String] shouldBe "generating"
    (json \ "history synchronization status").as[String] shouldBe "synced"

    (getRequest("/scorex/version") \ "version").as[String] should (startWith("Scorex") and include("v."))
  }

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
    (getRequest("/peers/connected") \ "peers").as[List[List[String]]].size shouldBe 1
    (getRequest("/peers/all") \ "peers").as[List[List[String]]].size should be >= 1
  }

  def getRequest(us: String, peer: String = peerUrl(application)): JsObject = {
    val request = Http(url(peer + us).GET)
    val response = Await.result(request, 10.seconds)
    Json.parse(response.getResponseBody).as[JsObject]
  }
}
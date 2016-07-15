package scorex.waves

import org.scalatest.{BeforeAndAfterAll, DoNotDiscover, FunSuite, Matchers}
import scorex.api.http.ApiKeyNotValid

@DoNotDiscover
class ScorexAPISpecification extends FunSuite with Matchers with BeforeAndAfterAll {

  import TestingCommons._

  override def beforeAll: Unit = {
    start()
  }

  override def afterAll: Unit = {
    stop()
  }

  test("/scorex/status API route") {
    val status = getRequest("/scorex/status")
    List("generating", "syncing") should contain((status \ "blockGeneratorStatus").as[String])
    List("synced", "syncing") should contain((status \ "historySynchronizationStatus").as[String])
  }

  test("/scorex/version API route") {
    val version = getRequest("/scorex/version")
    (version \ "version").as[String].contains("Waves") shouldBe true
    (version \ "version").as[String].contains("Release0") shouldBe true
    (version \ "version").as[String].contains("v0.2.") shouldBe true
  }

  test("/scorex/stop API route protected by api key") {
    val wrongKeyResponse = postRequest(us = "/scorex/stop", headers = Map("api_key" -> "wrong")).toString
    assert(wrongKeyResponse == ApiKeyNotValid.json.toString(), s"$wrongKeyResponse == ${ApiKeyNotValid.json.toString()} is false")
  }
}
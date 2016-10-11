package scorex.waves

import org.scalatest.{BeforeAndAfterAll, DoNotDiscover, FunSuite, Matchers}
import scorex.api.http.ApiKeyNotValid

@DoNotDiscover
class NodeAPISpecification extends FunSuite with Matchers with BeforeAndAfterAll {

  import TestingCommons._

  override def beforeAll: Unit = {
    start()
  }

  override def afterAll: Unit = {
    stop()
  }

  test("/node/status API route") {
    val status = getRequest("/node/status")
    List("generating", "suspended") should contain((status \ "blockGeneratorStatus").as[String])
    List("synced", "syncing", "idle") should contain((status \ "historySynchronizationStatus").as[String])
  }

  test("/node/version API route") {
    val version = getRequest("/node/version")
    (version \ "version").as[String].contains("Waves") shouldBe true
    (version \ "version").as[String].contains("Release0") shouldBe true
    (version \ "version").as[String].contains("v0.3.") shouldBe true
  }

  test("/node/stop API route protected by api key") {
    val wrongKeyResponse = postRequest(us = "/node/stop", headers = Map("api_key" -> "wrong")).toString
    assert(wrongKeyResponse == ApiKeyNotValid.json.toString(), s"$wrongKeyResponse == ${ApiKeyNotValid.json.toString()} is false")
  }
}
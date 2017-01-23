package scorex.waves

import org.scalatest.{FunSuite, Matchers}
import scorex.api.http.ApiKeyNotValid

class NodeAPISpecification extends FunSuite with Matchers with scorex.waves.TestingCommons {

  test("/node/status API route") {
    val status = getRequest("/node/status")
    List("generating", "suspended") should contain((status \ "blockGeneratorStatus").as[String])
    List("synced", "syncing", "idle") should contain((status \ "historySynchronizationStatus").as[String])
  }

  test("/node/version API route") {
    val version = getRequest("/node/version")
    (version \ "version").as[String].contains("Waves") shouldBe true
    (version \ "version").as[String].contains("TestRelease") shouldBe true
    (version \ "version").as[String].contains("v0.0.0") shouldBe true
  }

  test("/node/stop API route protected by api key") {
    val wrongKeyResponse = postRequest(us = "/node/stop", headers = Map("api_key" -> "wrong")).toString
    assert(wrongKeyResponse == ApiKeyNotValid.json.toString(), s"$wrongKeyResponse == ${ApiKeyNotValid.json.toString()} is false")
  }
}
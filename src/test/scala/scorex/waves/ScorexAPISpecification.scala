package scorex.waves

import org.scalatest.{FunSuite, Matchers}

class ScorexAPISpecification extends FunSuite with Matchers {

  import TestingCommons._

  test("/scorex/status API route") {
    val status = getRequest("/scorex/status")
    List("generating", "syncing") should contain((status \ "blockGeneratorStatus").as[String])
    List("synced", "syncing") should contain((status \ "historySynchronizationStatus").as[String])
  }

  test("/scorex/version API route") {
    val version = getRequest("/scorex/version")
    (version \ "version").as[String].contains("Waves") shouldBe true
    (version \ "version").as[String].contains("Release0") shouldBe true
    (version \ "version").as[String].contains("v. 0.2.") shouldBe true
  }
}
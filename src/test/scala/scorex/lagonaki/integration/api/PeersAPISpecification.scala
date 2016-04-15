package scorex.lagonaki.integration.api

import org.scalatest.{FunSuite, Matchers}

class PeersAPISpecification extends FunSuite with Matchers {

  import scorex.lagonaki.TestingCommons._

  test("/peers/connected API route") {
    val connected = getRequest("/peers/connected")
    (connected \\ "declaredAddress").toList.size should be >= 1
    (connected \\ "peerName").toList.size should be >= 1
    (connected \\ "peerNonce").toList.size should be >= 1
  }

  test("/peers/all API route") {
    val all = getRequest("/peers/all")
    (all \\ "address").toList.size should be >= 1
    (all \\ "nodeName").toList.size should be >= 1
    (all \\ "nodeNonce").toList.size should be >= 1
  }

  test("/peers/blacklisted API route") {
    val blacklisted = getRequest("/peers/blacklisted")
    blacklisted.toString() shouldBe "[]"
  }

  test("/peers/connect API route") {
    //TODO
  }

}
package scorex.waves

import org.scalatest.{FunSuite, Matchers}


class DebugAPISpecification extends FunSuite with Matchers {

  import TestingCommons._

  test("/debug/state") {
    val state = getRequest("/debug/state")
    (state \ "3P9NY5HLTKGeyGTYw5Zdyc4XUEmxBoBpbNe").as[Long] should be > 0L
  }

  test("/debug/state/{height}") {
    val state = getRequest("/debug/state/1")
    (state \ "3P9NY5HLTKGeyGTYw5Zdyc4XUEmxBoBpbNe").as[Long] shouldBe 9999999500000000L
    (state \ "3P8JdJGYc7vaLu4UXUZc1iRLdzrkGtdCyJM").as[Long] shouldBe 100000000L
    (state \ "3PAGPDPqnGkyhcihyjMHe9v36Y4hkAh9yDy").as[Long] shouldBe 100000000L
    (state \ "3P9o3ZYwtHkaU1KxsKkFjJqJKS3dLHLC9oF").as[Long] shouldBe 100000000L
    (state \ "3PJaDyprvekvPXPuAtxrapacuDJopgJRaU3").as[Long] shouldBe 100000000L
    (state \ "3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J").as[Long] shouldBe 100000000L
  }

  test("/debug/info") {
    val info = getRequest("/debug/info")
    (info \ "stateHeight").as[Int] shouldBe application.blockStorage.history.height()
    (info \ "stateHash").asOpt[Int].isDefined shouldBe true
  }

  test("/debug/settings") {
    val info = getRequest("/debug/settings", headers = Map("api_key" -> "test"))
//    (info \ "p2p" \ "localOnly").as[Boolean] shouldBe true
//    (info \ "p2p" \ "bindAddress").as[String] shouldBe "127.0.0.1"
//    (info \ "p2p" \ "port").as[Int] shouldBe 9091
//    (info \ "rpcPort").as[Int] shouldBe 9092
  }
}
package scorex.waves

import org.scalatest.{FunSuite, Matchers}


class DebugAPISpecification extends FunSuite with Matchers {

  import TestingCommons._

  test("/debug/state") {
    val state = getRequest("/debug/state")
    (state \ "2nDESCmSiTbcuutek3nJHGKevzgkycxFH9Y").as[Long] should be > 0L
  }

  test("/debug/state/{height}") {
    val state = getRequest("/debug/state/1")
    (state \ "2nDESCmSiTbcuutek3nJHGKevzgkycxFH9Y").as[Long] shouldBe 9999999500000000L
    (state \ "2nGSYaXexawHYz4wnCBMLzeEtbCVZQSvKRr").as[Long] shouldBe 100000000L
    (state \ "2nQRK9oxzYwg364Cur3N4uqeeFB9U7VDGcV").as[Long] shouldBe 100000000L
    (state \ "2nJHZ17iWSjMsZgCTCxUPoGzeX7hArqoyky").as[Long] shouldBe 100000000L
    (state \ "2n6CbQ2cufQ2ZkmMU3TzGGYa2AGKveqJjSd").as[Long] shouldBe 100000000L
    (state \ "2n7LeuEiZ4Te1zphx3gqGziuNJQ4gL4f2kt").as[Long] shouldBe 100000000L
  }

  test("/debug/info") {
    val info = getRequest("/debug/info")
    (info \ "stateHeight").as[Int] shouldBe application.blockStorage.history.height()
    (info \ "stateHash").asOpt[Int].isDefined shouldBe true
  }

  test("/debug/settings") {
    val info = getRequest("/debug/settings", headers = Map("api_key" -> "test"))
    (info \ "p2p" \ "localOnly").as[Boolean] shouldBe true
    (info \ "p2p" \ "bindAddress").as[String] shouldBe "127.0.0.1"
    (info \ "p2p" \ "port").as[Int] shouldBe 9091
    (info \ "rpcPort").as[Int] shouldBe 9092
  }


}
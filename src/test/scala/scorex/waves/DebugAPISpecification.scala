package scorex.waves

import org.scalatest.{FunSuite, Matchers}

class DebugAPISpecification extends FunSuite with Matchers with scorex.waves.TestingCommons {

  test("/debug/state") {
    val state = GET.requestJson("/debug/state")
    (state \ "3N3keodUiS8WLEw9W4BKDNxgNdUpwSnpb3K").as[Long] should be > 0L
  }

  test("/debug/info") {
    val info = GET.requestJson("/debug/info")
    (info \ "stateHeight").as[Int] shouldBe application.blockStorage.history.height()
    (info \ "stateHash").asOpt[Int].isDefined shouldBe true
  }
}

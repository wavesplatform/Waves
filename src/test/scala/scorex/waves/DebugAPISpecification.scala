package scorex.waves

import org.scalatest.{BeforeAndAfterAll, DoNotDiscover, FunSuite, Matchers}

@DoNotDiscover
class DebugAPISpecification extends FunSuite with Matchers with BeforeAndAfterAll {

  import TestingCommons._

  override def beforeAll: Unit = {
    start()
  }

  override def afterAll: Unit = {
    stop()
  }

  test("/debug/state") {
    val state = getRequest("/debug/state")
    (state \ "3N5jhcA7R98AUN12ee9pB7unvnAKfzb3nen").as[Long] should be > 0L
  }

  test("/debug/state/{height}") {
    val state = getRequest("/debug/state/1")
    (state \ "3N5jhcA7R98AUN12ee9pB7unvnAKfzb3nen").as[Long] shouldBe 9999999500000000L
    (state \ "3MyTvqfeLWkvjSZ1hwkhQjzipZr7Pk8dyMR").as[Long] shouldBe 100000000L
    (state \ "3MqS3mVY4Yr4HoTdpWiEaq9phwbaoWS2W6A").as[Long] shouldBe 100000000L
    (state \ "3N3CDuzGXB2qP5vb2NvnnDQ68HahNCfYVBg").as[Long] shouldBe 100000000L
    (state \ "3N2sacZ9XTQUkLDdZZgtb1zJUAmr6oziRrU").as[Long] shouldBe 100000000L
    (state \ "3N189PMB8BaxngN3fNvDRkFbvbH8xMkk328").as[Long] shouldBe 100000000L
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
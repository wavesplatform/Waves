package scorex.lagonaki.integration.api

import org.scalatest.{FunSuite, Matchers}
import scorex.crypto.encode.Base58

import scala.util.Random

class UtilsAPISpecification extends FunSuite with Matchers {

  import scorex.lagonaki.TestingCommons._

  test("/utils/hash API route") {
  //TODO
  }

  test("/utils/seed API route") {
    Base58.decode((getRequest("/utils/seed") \ "seed").as[String]).isSuccess shouldBe true
  }

  test("/utils/seed/{length} API route") {
    val length = Random.nextInt(4096)
    Base58.decode((getRequest(s"/utils/seed/$length") \ "seed").as[String]).get.length shouldBe length
  }
}
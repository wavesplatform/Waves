package scorex.lagonaki.integration.api

import org.scalatest.{FunSuite, Matchers}
import scorex.crypto.encode.Base58

import scala.util.Random

class SeedAPISpecification extends FunSuite with Matchers {

  import scorex.lagonaki.TestingCommons._

  test("/seed API route") {
    Base58.decode((getRequest("/seed/") \ "seed").as[String]).isSuccess shouldBe true
  }

  test("/seed/{length} API route") {
    val length = Random.nextInt(4096)
    Base58.decode((getRequest(s"/seed/$length") \ "seed").as[String]).get.length shouldBe length
  }
}
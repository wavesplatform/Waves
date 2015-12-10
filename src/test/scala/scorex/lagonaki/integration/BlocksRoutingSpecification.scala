package scorex.lagonaki.integration

import play.api.libs.json.Json
import scorex.api.http.BlocksApiRoute
import scorex.lagonaki.TestingCommons


class BlocksRoutingSpecification extends RouteTest {

  import TestingCommons._

  application.checkGenesis()

  val blocksRoute = BlocksApiRoute(application.blockStorage.history, application.wallet).route

  "blocksRouting" should "return first block" in {
    Get("/blocks/first") ~> blocksRoute ~> check {
      val js = Json.parse(responseAs[String])
      (js \ "fee").as[Int] shouldBe 0
      (js \ "version").as[Int] should be >= 1
      (js \ "transactions").toOption should not be None
      //TODO check concrete block?
    }
  }

  it should "return last block" in {
    Get("/blocks/last") ~> blocksRoute ~> check {
      val js = Json.parse(responseAs[String])
      (js \ "fee").as[Int] should be >= 0
      (js \ "version").as[Int] should be >= 1
      (js \ "transactions").toOption should not be None
    }
  }
  it should "return error for wrong signature" in {
    Get("/blocks/signature/wrongSignature") ~> blocksRoute ~> check {
      val js = Json.parse(responseAs[String])
      (js \ "error").as[Int] shouldBe 301
    }
  }

  //TODO check correct signature
  it should "return block at 1" in {
    Get("/blocks/at/1") ~> blocksRoute ~> check {
      val js = Json.parse(responseAs[String])
      (js \ "fee").as[Int] should be >= 0
      (js \ "version").as[Int] should be >= 1
    }
  }

  it should "return height" in {
    Get("/blocks/height") ~> blocksRoute ~> check {
      val js = Json.parse(responseAs[String])
      (js \ "height").as[Int] should be >= 1
    }
  }

  //TODO test route /blocks/height/$encodedSignature
  //TODO test route /blocks/child/$encodedSignature
  //TODO test route /blocks/address/$address
}
package scorex.lagonaki.integration

import play.api.libs.json.Json
import scorex.api.http.BlocksApiRoute
import scorex.block.Block
import scorex.lagonaki.TestingCommons


class BlocksRoutingSpecification extends RouteTest {

  import TestingCommons._

  application.checkGenesis()
  implicit val consensusModule = application.consensusModule
  implicit val transactionModule = application.transactionModule
  lazy val genesis = Block.genesis()
  lazy val signature = (genesis.json \ "signature").as[String]

  val blocksRoute = BlocksApiRoute(application).route

  "blocksRouting" should "return first block" in {
    Get("/blocks/first") ~> blocksRoute ~> check {
      val js = Json.parse(responseAs[String])
      (js \ "fee").as[Int] shouldBe 0
      (js \ "version").as[Int] should be >= 1
      (js \ "transactions").toOption should not be None
      js.toString() shouldBe genesis.json.toString()
    }
  }

  it should "return block for correct signature" in {
    Get(s"/blocks/signature/$signature") ~> blocksRoute ~> check {
      val js = Json.parse(responseAs[String])
      js.toString() shouldBe genesis.json.toString()
    }
  }

  it should "return block height for correct signature" in {
    Get(s"/blocks/height/$signature") ~> blocksRoute ~> check {
      val js = Json.parse(responseAs[String])
      (js \ "height").as[Int] shouldBe 1
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

  it should "return block at 1" in {
    Get("/blocks/at/1") ~> blocksRoute ~> check {
      val js = Json.parse(responseAs[String])
      (js \ "fee").as[Int] should be >= 0
      (js \ "version").as[Int] should be >= 1
      js.toString() shouldBe genesis.json.toString()
    }
  }

  it should "return height" in {
    Get("/blocks/height") ~> blocksRoute ~> check {
      val js = Json.parse(responseAs[String])
      (js \ "height").as[Int] should be >= 1
    }
  }

  it should "return return error when there are no children" in {
    val sig = transactionModule.blockStorage.history.lastBlock.encodedId
    Get(s"/blocks/child/$sig") ~> blocksRoute ~> check {
      val js = Json.parse(responseAs[String])
      (js \ "status").as[String] shouldBe "error"
    }
  }

  //TODO test route /blocks/child/$encodedSignature
  //TODO test route /blocks/address/$address
  //TODO test blocks other then genesis
}
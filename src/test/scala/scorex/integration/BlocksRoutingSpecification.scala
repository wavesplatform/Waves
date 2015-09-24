package scorex.integration

import scorex.api.http.BlocksHttpService

//todo: uncomment/fix

class BlocksRoutingSpecification extends RouteTest {
  /* override implicit val application = new LagonakiApplication("settings-test.json")

   application.checkGenesis()

   "blocksRouting" should "return first block" in {
     Get("/blocks/first") ~> blocksRouting ~> check {
       val js = Json.parse(responseAs[String])
       (js \ "fee").as[Int] shouldBe 0
       (js \ "version").as[Int] should be >= 1
       (js \ "transactions").toOption should not be None
       //TODO check concrete block?
     }
   }

   it should "return last block" in {
     Get("/blocks/last") ~> blocksRouting ~> check {
       val js = Json.parse(responseAs[String])
       (js \ "fee").as[Int] should be >= 0
       (js \ "version").as[Int] should be >= 1
       (js \ "transactions").toOption should not be None
     }
   }
   it should "return error for wrong signature" in {
     Get("/blocks/signature/wrongSignature") ~> blocksRouting ~> check {
       val js = Json.parse(responseAs[String])
       (js \ "error").as[Int] shouldBe 301
     }
   }

   //TODO check correct signature
   it should "return block at 1" in {
     Get("/blocks/at/1") ~> blocksRouting ~> check {
       val js = Json.parse(responseAs[String])
       (js \ "fee").as[Int] should be >= 0
       (js \ "version").as[Int] should be >= 1
     }
   }

   it should "return height" in {
     Get("/blocks/height") ~> blocksRouting ~> check {
       val js = Json.parse(responseAs[String])
       (js \ "height").as[Int] should be >= 1
     }
   }                                                                                  */

  //TODO test route /blocks/height/$encodedSignature
  //TODO test route /blocks/child/$encodedSignature
  //TODO test route /blocks/address/$address
}
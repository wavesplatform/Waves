package scorex.integration

//todo: uncomment/fix
class AdressesRoutingSpecification extends RouteTest {
  /*  override implicit val application = new LagonakiApplication("settings-test.json")

    "adressesRouting" should "handle root request" in {
      Get("/addresses/") ~> adressesRouting ~> check {
        val js = Json.parse(responseAs[String])
      }
    }
    "adressesRouting" should "return correct new address" in {
      var address: Option[String] = None
      Get("/addresses/new") ~> adressesRouting ~> check {
        val js = Json.parse(responseAs[String])
        address = (js \ "address").toOption.map(_.toString()).map(s => s.slice(1, s.length - 1))
        address should not be None
      }
      Get("/addresses/validate/" + address.get) ~> adressesRouting ~> check {
        val js = Json.parse(responseAs[String])
        (js \ "valid").as[Boolean] shouldBe true
      }

    }                                                                                  */
}
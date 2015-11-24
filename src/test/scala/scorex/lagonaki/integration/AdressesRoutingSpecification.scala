package scorex.lagonaki.integration

import play.api.libs.json.Json
import scorex.api.http.AddressApiRoute
import scorex.lagonaki.server.LagonakiApplication


class AdressesRoutingSpecification extends RouteTest {

  val application = new LagonakiApplication("settings-test.json")
  application.checkGenesis()
  val adressesRoute = AddressApiRoute(application.wallet, application.state).route

  "adressesRouting" should "handle root request" in {
    Get("/addresses/") ~> adressesRoute ~> check {
      val js = Json.parse(responseAs[String])
    }
  }

  "adressesRouting" should "return correct new address" in {
    var address: Option[String] = None
    Get("/addresses/new") ~> adressesRoute ~> check {
      val js = Json.parse(responseAs[String])
      address = (js \ "address").toOption.map(_.toString()).map(s => s.slice(1, s.length - 1))
      address should not be None
    }
    Get("/addresses/validate/" + address.get) ~> adressesRoute ~> check {
      val js = Json.parse(responseAs[String])
      (js \ "valid").as[Boolean] shouldBe true
    }
  }
}
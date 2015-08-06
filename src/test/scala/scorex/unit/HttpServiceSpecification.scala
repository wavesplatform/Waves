package scorex.unit

import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json.Json
import scorex.api.http.AddressHttpService
import spray.routing.HttpService
import spray.testkit.ScalatestRouteTest

class HttpServiceSpecification extends FlatSpec with ScalatestRouteTest with HttpService with Matchers with AddressHttpService {
  def actorRefFactory = system

  "adressesRouting" should "handle root request" in {
    Get("/addresses/") ~> adressesRouting ~> check {
      println(responseAs[String])
    }
  }
  //  it should "handle new request" in {
  //    Get("/addresses/new") ~> adressesRouting ~> check {
  //      Json.parse(responseAs[String])
  //    }
  //  }
}
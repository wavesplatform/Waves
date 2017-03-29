package com.wavesplatform.http

import akka.testkit.TestProbe
import com.wavesplatform.http.ApiMarshallers._
import play.api.libs.json.{Format, Json}
import scorex.api.http.{ApiKeyNotValid, PeersApiRoute}

class PeersRouteSpec extends RouteSpec("/peers") with RestAPISettingsHelper {
  import PeersRouteSpec._

  private val peerManager = TestProbe()
  private val networkController = TestProbe()
  private val route = PeersApiRoute(restAPISettings, peerManager.ref, networkController.ref).route

  routePath("/connected") in {

  }

  routePath("/all") in pending
  routePath("/connect") in {
    val req = ConnectReq("8.8.8.8", 1488)
    Post(routePath("/connect"), req) ~> route should produce (ApiKeyNotValid)
    Post(routePath("/connect"), req) ~> api_key(apiKey) ~> route ~> check {
      println(response)
      val resp = responseAs[ConnectResp]
      resp.status shouldBe "Trying to connect"
      resp.hostname shouldBe Some(req.host)
    }
  }

  routePath("/blacklisted") in {

  }
}

object PeersRouteSpec {
  case class ConnectReq(host: String, port: Int)
  implicit val connectReqFormat: Format[ConnectReq] = Json.format

  case class ConnectResp(status: String, hostname: Option[String])
  implicit val connectRespFormat: Format[ConnectResp] = Json.format
}

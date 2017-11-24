package com.wavesplatform.http

import com.wavesplatform.TestWallet
import com.wavesplatform.http.ApiMarshallers._
import play.api.libs.json.JsObject
import scorex.api.http.{ApiKeyNotValid, WalletApiRoute}
import scorex.crypto.encode.Base58

class WalletRouteSpec extends RouteSpec("/wallet") with RestAPISettingsHelper with TestWallet {
  private val route = WalletApiRoute(restAPISettings, testWallet).route

  routePath("/seed") - {
    "requires api_key header" in {
      Get(routePath("/seed")) ~> route should produce(ApiKeyNotValid)
    }

    "returns seed when api_key header is present" in {
      Get(routePath("/seed")) ~> api_key(apiKey) ~> route ~> check {
        (responseAs[JsObject] \ "seed").as[String] shouldEqual Base58.encode(testWallet.seed.get)
      }
    }
  }
}

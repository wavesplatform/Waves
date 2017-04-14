package com.wavesplatform.http

import com.wavesplatform.http.ApiMarshallers._
import play.api.libs.json.JsObject
import scorex.api.http.{ApiKeyNotValid, WalletApiRoute}
import scorex.crypto.encode.Base58
import scorex.wallet.Wallet

class WalletRouteSpec extends RouteSpec("/wallet") with RestAPISettingsHelper {
  private val wallet = {
    val file = scorex.createTestTemporaryFile("wallet", ".dat")
    val wallet = new Wallet(Some(file.getCanonicalPath), "123", None)
    wallet.generateNewAccounts(10)
    wallet
  }

  private val route = WalletApiRoute(restAPISettings, wallet).route

  routePath("/seed") - {
    "requires api_key header" in {
      Get(routePath("/seed")) ~> route should produce(ApiKeyNotValid)
    }

    "returns seed when api_key header is present" in {
      Get(routePath("/seed")) ~> api_key(apiKey) ~> route ~> check {
        (responseAs[JsObject] \ "seed").as[String] shouldEqual Base58.encode(wallet.seed)
      }
    }
  }
}

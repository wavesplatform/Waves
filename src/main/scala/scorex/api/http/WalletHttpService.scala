package scorex.api.http

import scorex.controller.Controller
import play.api.libs.json.Json
import scorex.crypto.Base58
import spray.routing.HttpService


trait WalletHttpService extends HttpService with CommonApiFunctions {

  import Controller.wallet

  lazy val walletRouting = {
    pathPrefix("wallet") {
      path("") {
        get {
          complete(Json.obj("exists" -> wallet.exists()).toString())
        }
      } ~ path("seed") {
        get {
          complete {
            lazy val seedJs = Json.obj("seed" -> Base58.encode(wallet.exportSeed()))
            walletNotExists().getOrElse(seedJs).toString()
          }
        }
      } /* todo: fix or remove ~ path("create") {
        post {
          entity(as[String]) { body => complete {
            Try {
              walletExists().getOrElse {
                val js = Json.parse(body)
                val recover = (js \ "recover").as[Boolean]
                val seed = (js \ "seed").as[String]
                val password = (js \ "password").as[String]
                val amount = (js \ "amount").as[Int]

                Try(Base58.decode(seed)) match {
                  case Success(seedBytes) if seedBytes.length == 32 =>
                    if (amount < 1) ApiError.toJson(ApiError.ERROR_INVALID_AMOUNT)
                    else {
                      val res = if (recover) wallet.create(seedBytes, password, amount)
                      else wallet.create(seedBytes, password, amount)
                      Json.obj("success" -> res)
                    }
                  case _ => ApiError.toJson(ApiError.ERROR_INVALID_SEED)
                }
              }
            }.getOrElse(ApiError.toJson(ApiError.ERROR_JSON)).toString()
          }
          }
        }
      } */
    }
  }
}

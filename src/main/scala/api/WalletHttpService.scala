package api

import controller.Controller
import play.api.libs.json.Json
import scorex.crypto.Base58
import scorex.wallet.Wallet
import spray.routing.HttpService

import scala.util.{Success, Try}


trait WalletHttpService extends HttpService with CommonApifunctions {

  lazy val walletRouting = {
    pathPrefix("wallet") {
      path("") {
        get {
          complete(Json.obj("unlocked" -> Wallet.isUnlocked).toString())
        }
      } ~ path("seed") {
        get {
          complete {
            lazy val seedJs = Json.obj("seed" -> Base58.encode(Wallet.exportSeed().get))
            walletNotExistsOrLocked().getOrElse(seedJs).toString()
          }
        }
      } ~ path("lock") {
        get {
          complete {
            walletNotExistsOrLocked().getOrElse(Json.obj("locked" -> Wallet.lock())).toString()
          }
        }
      } ~ path("unlock") {
        post {
          entity(as[String]) { body => complete {
            val password = body
            walletNotExistsOrLocked().getOrElse(Json.obj("unlocked" -> Wallet.unlock(password))).toString()
          }
          }
        }
      } ~ path("create") {
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
                      val res = if (recover) Controller.recoverWallet(seedBytes, password, amount)
                      else Controller.createWallet(seedBytes, password, amount)
                      Json.obj("success" -> res)
                    }
                  case _ => ApiError.toJson(ApiError.ERROR_INVALID_SEED)
                }
              }
            }.getOrElse(ApiError.toJson(ApiError.ERROR_JSON)).toString()
          }
          }
        }
      }
    }
  }
}

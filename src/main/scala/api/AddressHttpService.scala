package api

import java.nio.charset.StandardCharsets

import akka.actor.Actor
import controller.Controller
import play.api.libs.json.Json
import scorex.account.{PublicKeyAccount, Account}
import scorex.crypto.{Base58, Crypto}
import spray.routing.HttpService

import scala.util.{Success, Failure, Try}


class AddressHttpServiceActor extends Actor with AddressHttpService {
  override def actorRefFactory = context

  override def receive = runRoute(route)
}

trait AddressHttpService extends HttpService with CommonApifunctions {
  private def balanceJson(address: String, confirmations: Int) =
    if (!Crypto.isValidAddress(address)) {
      ApiError.toJson(ApiError.ERROR_INVALID_ADDRESS)
    } else {
      Json.obj(
        "address" -> address,
        "confirmations" -> confirmations,
        "balance" -> new Account(address).getBalance(confirmations).toPlainString
      )
    }

  lazy val route =
    path("/") {
      get {
        //CHECK IF WALLET EXISTS
        val jsRes = if (!Controller.doesWalletExists) {
          ApiError.toJson(ApiError.ERROR_WALLET_NO_EXISTS)
        } else {
          //GET ACCOUNTS
          val accounts = Controller.accounts()
          val addresses = accounts.map(_.address)
          Json.arr(addresses)
        }
        complete(Json.stringify(jsRes))
      }
    } ~ path("validate" / Segment) { case address =>
      get {
        val jsRes = Json.obj("address" -> address, "valid" -> Crypto.isValidAddress(address))
        complete(Json.stringify(jsRes))
      }
    } ~ path("seed" / Segment) { case address =>
      get {
        //CHECK IF WALLET EXISTS
        val jsRes = walletNotExistsOrLocked().getOrElse {
          if (!Crypto.isValidAddress(address)) {
            ApiError.toJson(ApiError.ERROR_INVALID_ADDRESS)
          } else {
            Controller.accountByAddress(address) match {
              case None => ApiError.toJson(ApiError.ERROR_WALLET_ADDRESS_NO_EXISTS)
              case Some(account) =>
                Controller.exportAccountSeed(address) match {
                  case None => ApiError.toJson(ApiError.ERROR_WALLET_SEED_EXPORT_FAILED)
                  case Some(seed) =>
                    Json.obj("address" -> address, "seed" -> Base58.encode(seed))
                }
            }
          }
        }
        complete(Json.stringify(jsRes))
      }
    } ~ path("new") {
      get {
        complete {
          walletNotExistsOrLocked().getOrElse(Json.obj("address" -> Controller.generateNewAccount())).toString()
        }
      }
    } ~ path("balance" / Segment / IntNumber) { case (address, confirmations) =>
      get {
        val jsRes = balanceJson(address, confirmations)
        complete(Json.stringify(jsRes))
      }
    } ~ path("balance" / Segment) { case address =>
      get {
        val jsRes = balanceJson(address, 1)
        complete(Json.stringify(jsRes))
      }
    } ~ path("generatingbalance" / Segment) { case address =>
      get {
        val jsRes = if (!Crypto.isValidAddress(address)) {
          ApiError.toJson(ApiError.ERROR_INVALID_ADDRESS)
        } else {
          Json.obj(
            "address" -> address,
            "balance" -> new Account(address).getGeneratingBalance().toPlainString
          )
        }
        complete(Json.stringify(jsRes))
      }
    } ~ path("/") {
      post {
        entity(as[String]) { seed =>
          if (seed.isEmpty) {
            val jsRes = walletNotExistsOrLocked().getOrElse {
              Json.obj("address" -> Controller.generateNewAccount)
            }
            complete(Json.stringify(jsRes))
          } else {
            val jsRes = walletNotExistsOrLocked().getOrElse {
              //DECODE SEED
              Try(Base58.decode(seed)).toOption.flatMap { seedBytes =>
                if (seedBytes != null && seedBytes.size == 32) {
                  Some(Json.obj("address" -> Controller.importAccountSeed(seedBytes)))
                } else None
              }.getOrElse(ApiError.toJson(ApiError.ERROR_INVALID_SEED))
            }
            complete(Json.stringify(jsRes))
          }
        }
      }
    } ~ path("verify" / Segment) { case address =>
      post {
        entity(as[String]) { jsText =>
          val jsRes = Try {
            val js = Json.parse(jsText)
            val msg = (js \ "message").as[String]
            val signature = (js \ "signature").as[String]
            val pubKey = (js \ "publickey").as[String]

            if (!Crypto.isValidAddress(address)) {
              ApiError.toJson(ApiError.ERROR_INVALID_ADDRESS)
            } else {
              //DECODE SIGNATURE
              (Try(Base58.decode(signature)), Try(Base58.decode(pubKey))) match {
                case (Failure(_), _) => ApiError.toJson(ApiError.ERROR_INVALID_SIGNATURE)
                case (_, Failure(_)) => ApiError.toJson(ApiError.ERROR_INVALID_PUBLIC_KEY)
                case (Success(signatureBytes), Success(pubKeyBytes)) =>
                  val account = new PublicKeyAccount(pubKeyBytes)
                  val isValid = account.address == address &&
                    Crypto.verify(pubKeyBytes, signatureBytes, msg.getBytes(StandardCharsets.UTF_8))
                  Json.obj("valid" -> isValid)
              }
            }
          }.getOrElse(ApiError.toJson(ApiError.ERROR_JSON))
          complete(Json.stringify(jsRes))
        }
      }
    } ~ path("sign" / Segment) { case address =>
      post {
        entity(as[String]) { message =>
          val jsRes = walletNotExistsOrLocked().getOrElse {
            if (!Crypto.isValidAddress(address)) {
              ApiError.toJson(ApiError.ERROR_INVALID_ADDRESS)
            } else {
              Controller.privateKeyAccountByAddress(address) match {
                case None => ApiError.toJson(ApiError.ERROR_WALLET_ADDRESS_NO_EXISTS)
                case Some(account) =>
                  Json.obj("message" -> message,
                    "publickey" -> Base58.encode(account.publicKey),
                    "signature" -> Base58.encode(Crypto.sign(account, message.getBytes(StandardCharsets.UTF_8))))
              }
            }
          }
          complete(jsRes.toString())
        }
      }
    } ~ path("/" / Segment) { case address =>
      delete {
        val jsRes = walletNotExistsOrLocked().getOrElse {
          if (!Crypto.isValidAddress(address)) {
            ApiError.toJson(ApiError.ERROR_INVALID_ADDRESS)
          } else {
            val deleted = Controller.privateKeyAccountByAddress(address).exists(account =>
              Controller.deleteAccount(account))
            Json.obj("deleted" -> deleted)
          }
        }
        complete(jsRes.toString())
      }
    }
}
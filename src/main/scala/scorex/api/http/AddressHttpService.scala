package scorex.api.http

import java.nio.charset.StandardCharsets
import play.api.libs.json.Json
import scorex.Controller
import scorex.account.{Account, PublicKeyAccount}
import scorex.crypto.{Base58, Crypto}
import spray.routing.HttpService

import scala.util.{Failure, Success, Try}


trait AddressHttpService extends HttpService with CommonApiFunctions {

  import Controller.wallet

  lazy val adressesRouting =
    pathPrefix("addresses") {
      path("") {
        get {
          complete {
            val addresses = wallet.privateKeyAccounts().map(_.address)
            Json.arr(addresses).toString()
          }
        }
      } ~ path("validate" / Segment) { case address =>
        get {
          complete {
            val jsRes = Json.obj("address" -> address, "valid" -> Account.isValidAddress(address))
            Json.stringify(jsRes)
          }
        }
      } ~ path("seed" / Segment) { case address =>
        get {
          complete {
            //CHECK IF WALLET EXISTS
            val jsRes = withAccount(address) { account =>
              wallet.exportAccountSeed(account.address) match {
                case None => ApiError.json(ApiError.WalletSeedExportFailed)
                case Some(seed) => Json.obj("address" -> address, "seed" -> Base58.encode(seed))
              }
            }
            Json.stringify(jsRes)
          }
        }
      } ~ path("new") {
        get {
          complete {
            walletNotExists().getOrElse {
              wallet.generateNewAccount() match {
                case Some(pka) => Json.obj("address" -> pka.address)
                case None => ApiError.json(ApiError.Unknown)
              }

            }.toString()
          }
        }
      } ~ path("balance" / Segment / IntNumber) { case (address, confirmations) =>
        //todo: confirmations parameter doesn't work atm
        get {
          complete {
            val jsRes = balanceJson(address, confirmations)
            Json.stringify(jsRes)
          }
        }
      } ~ path("balance" / Segment) { case address =>
        get {
          complete {
            val jsRes = balanceJson(address, 1)
            Json.stringify(jsRes)
          }
        }
      } ~ path("generatingbalance" / Segment) { case address =>
        get {
          complete {
            val jsRes = if (!Account.isValidAddress(address)) {
              ApiError.json(ApiError.InvalidAddress)
            } else {
              Json.obj(
                "address" -> address,
                "balance" -> Controller.blockchainStorage.generationBalance(address)
              )
            }
            Json.stringify(jsRes)
          }
        }
      } ~ path("verify" / Segment) { case address =>
        post {
          entity(as[String]) { jsText =>
            complete {
              val jsRes = Try {
                val js = Json.parse(jsText)
                val msg = (js \ "message").as[String]
                val signature = (js \ "signature").as[String]
                val pubKey = (js \ "publickey").as[String]

                if (!Account.isValidAddress(address)) {
                  ApiError.json(ApiError.InvalidAddress)
                } else {
                  //DECODE SIGNATURE
                  (Base58.decode(msg), Base58.decode(signature), Base58.decode(pubKey)) match {
                    case (Failure(_), _, _) => ApiError.json(ApiError.InvalidMessage)
                    case (_, Failure(_), _) => ApiError.json(ApiError.InvalidSignature)
                    case (_, _, Failure(_)) => ApiError.json(ApiError.InvalidPublicKey)
                    case (Success(msgBytes), Success(signatureBytes), Success(pubKeyBytes)) =>
                      val account = new PublicKeyAccount(pubKeyBytes)
                      val isValid = account.address == address &&
                        Crypto.verify(signatureBytes, msgBytes, pubKeyBytes)
                      Json.obj("valid" -> isValid)
                  }
                }
              }.getOrElse(ApiError.json(ApiError.WrongJson))
              Json.stringify(jsRes)
            }
          }
        }
      } ~ path("sign" / Segment) { case address =>
        post {
          entity(as[String]) { message =>
            complete {
              val jsRes = walletNotExists().getOrElse {
                if (!Account.isValidAddress(address)) {
                  ApiError.json(ApiError.InvalidAddress)
                } else {
                  wallet.privateKeyAccount(address) match {
                    case None => ApiError.json(ApiError.WalletAddressNotExists)
                    case Some(account) =>
                      Try(Crypto.sign(account, message.getBytes(StandardCharsets.UTF_8))) match {
                        case Success(signature) =>
                          Json.obj("message" -> message,
                            "publickey" -> Base58.encode(account.publicKey),
                            "signature" -> Base58.encode(signature))
                        case Failure(t) => ApiError.json(t)
                      }
                  }
                }
              }
              jsRes.toString()
            }
          }
        }
      } ~ path("address" / Segment) { case address => //todo: fix routing to that?
        delete {
          complete {
            val jsRes = walletNotExists().getOrElse {
              if (!Account.isValidAddress(address)) {
                ApiError.json(ApiError.InvalidAddress)
              } else {
                val deleted = wallet.privateKeyAccount(address).exists(account =>
                  wallet.deleteAccount(account))
                Json.obj("deleted" -> deleted)
              }
            }
            jsRes.toString()
          }
        }
      } /* todo: fix or remove ~ path("") {
        post {
          entity(as[String]) { seed =>
            complete {
              val jsRes = if (seed.isEmpty) {
                walletNotExists().getOrElse {
                  wallet.generateNewAccount() match {
                    case Some(pka) => Json.obj("address" -> pka.address)
                    case None => ApiError.toJson(ApiError.ERROR_UNKNOWN)
                  }
                }
              } else {
                walletNotExists().getOrElse {
                  //DECODE SEED
                  Try(Base58.decode(seed)).toOption.flatMap { seedBytes =>
                    if (seedBytes != null && seedBytes.size == 32) {
                      Some(Json.obj("address" -> wallet.importAccountSeed(seedBytes)))
                    } else None
                  }.getOrElse(ApiError.toJson(ApiError.ERROR_INVALID_SEED))
                }
              }
              Json.stringify(jsRes)
            }
          }
        }
      } */
    }

  private def balanceJson(address: String, confirmations: Int) =
    if (!Account.isValidAddress(address)) {
      ApiError.json(ApiError.InvalidAddress)
    } else {
      Json.obj(
        "address" -> address,
        "confirmations" -> confirmations,
        "balance" -> Controller.blockchainStorage.balance(address, confirmations)
      )
    }
}
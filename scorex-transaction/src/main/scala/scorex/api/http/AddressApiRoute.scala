package scorex.api.http

import java.nio.charset.StandardCharsets

import akka.actor.ActorRefFactory
import play.api.libs.json.Json
import scorex.account.{Account, PublicKeyAccount}
import scorex.crypto.{Base58, SigningFunctionsImpl}
import scorex.transaction.state.LagonakiState
import scorex.transaction.state.wallet.Wallet
import spray.routing.HttpService._

import scala.util.{Failure, Success, Try}


case class AddressApiRoute(wallet: Wallet, state: LagonakiState)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  implicit val w = wallet

  override lazy val route =
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
            val jsRes = withPrivateKeyAccount(address) { account =>
              wallet.exportAccountSeed(account.address) match {
                case None => WalletSeedExportFailed.json
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
                case None => Unknown.json
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
              InvalidAddress.json
            } else {
              Json.obj(
                "address" -> address,
                "balance" -> state.generationBalance(address)
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
                  InvalidAddress.json
                } else {
                  //DECODE SIGNATURE
                  (Base58.decode(msg), Base58.decode(signature), Base58.decode(pubKey)) match {
                    case (Failure(_), _, _) => InvalidMessage.json
                    case (_, Failure(_), _) => InvalidSignature.json
                    case (_, _, Failure(_)) => InvalidPublicKey.json
                    case (Success(msgBytes), Success(signatureBytes), Success(pubKeyBytes)) =>
                      val account = new PublicKeyAccount(pubKeyBytes)
                      val isValid = account.address == address &&
                        SigningFunctionsImpl.verify(signatureBytes, msgBytes, pubKeyBytes)
                      Json.obj("valid" -> isValid)
                  }
                }
              }.getOrElse(WrongJson.json)
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
                  InvalidAddress.json
                } else {
                  wallet.privateKeyAccount(address) match {
                    case None => WalletAddressNotExists.json
                    case Some(account) =>
                      Try(SigningFunctionsImpl.sign(account, message.getBytes(StandardCharsets.UTF_8))) match {
                        case Success(signature) =>
                          Json.obj("message" -> message,
                            "publickey" -> Base58.encode(account.publicKey),
                            "signature" -> Base58.encode(signature))
                        case Failure(t) => json(t)
                      }
                  }
                }
              }
              jsRes.toString()
            }
          }
        }
      } ~ path("address" / Segment) { case address =>
        delete {
          complete {
            val jsRes = walletNotExists().getOrElse {
              if (!Account.isValidAddress(address)) {
                InvalidAddress.json
              } else {
                val deleted = wallet.privateKeyAccount(address).exists(account =>
                  wallet.deleteAccount(account))
                Json.obj("deleted" -> deleted)
              }
            }
            jsRes.toString()
          }
        }
      } ~ path("create") {
        get {
          complete {
            val jsRes =
              walletNotExists().getOrElse {
                wallet.generateNewAccount() match {
                  case Some(pka) => Json.obj("address" -> pka.address)
                  case None => Unknown.json
                }
              }
            Json.stringify(jsRes)
          }
        }
      }
    }

  private def balanceJson(address: String, confirmations: Int) =
    if (!Account.isValidAddress(address)) {
      InvalidAddress.json
    } else {
      Json.obj(
        "address" -> address,
        "confirmations" -> confirmations,
        "balance" -> state.balance(address, confirmations)
      )
    }
}
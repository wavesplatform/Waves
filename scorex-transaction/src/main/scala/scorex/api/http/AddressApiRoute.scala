package scorex.api.http

import java.nio.charset.StandardCharsets
import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import com.wordnik.swagger.annotations._
import play.api.libs.json.Json
import scorex.account.{Account, PublicKeyAccount}
import scorex.crypto.{Base58, SigningFunctionsImpl}
import scorex.transaction.state.LagonakiState
import scorex.transaction.state.wallet.Wallet
import spray.http.MediaTypes._

import scala.util.{Failure, Success, Try}


@Api(value = "/addresses", description = "Info about wallet's accounts and other calls about addresses")
case class AddressApiRoute(wallet: Wallet, state: LagonakiState)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  implicit val w = wallet

  override lazy val route =
    pathPrefix("addresses") {
      root ~ validate ~ seed ~ confirmationBalance ~ balance ~ geenratingBalance ~ verify ~ sign ~ deleteAddress ~ create
    }

  @Path("/{address}")
  @ApiOperation(value = "Delete", notes = "Remove the account with address {address} from the wallet", httpMethod = "DELETE")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  def deleteAddress = {
    path(Segment) { case address =>
      jsonRoute({
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
      }, delete)
    }
  }

  @Path("/sign/{address}")
  @ApiOperation(value = "Sign", notes = "Sign a message with a private key associated with {address}", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "message", value = "Message to sign as a plain string", required = true, paramType = "body", dataType = "String"),
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  def sign = {
    path("sign" / Segment) { case address =>
      post {
        respondWithMediaType(`application/json`) {
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
      }
    }
  }

  //TODO add Message case class
  @Path("/verify/{address}")
  @ApiOperation(value = "Verify", notes = "Check a signature of a message signed by an account", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path"),
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "Message",
      defaultValue = "{\n\t\"message\":\"Base58-encoded message\",\n\t\"signature\":\"Base58-encoded signature\",\n\t\"publickey\":\"Base58-encoded public key\"\n}"
    )
  ))
  def verify = {
    path("verify" / Segment) { case address =>
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
    }
  }

  @Path("/generatingbalance/{address}")
  @ApiOperation(value = "Generating balance", notes = "Account's generating balance(the same as balance atm)", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  def geenratingBalance = {
    path("generatingbalance" / Segment) { case address =>
      jsonRoute {
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
  }

  @Path("/balance/{address}")
  @ApiOperation(value = "Balance", notes = "Account's balance", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  def balance = {
    path("balance" / Segment) { case address =>
      jsonRoute {
        val jsRes = balanceJson(address, 1)
        Json.stringify(jsRes)
      }
    }
  }

  //TODO describe
  def confirmationBalance = {
    path("balance" / Segment / IntNumber) { case (address, confirmations) =>
      //todo: confirmations parameter doesn't work atm
      jsonRoute {
        val jsRes = balanceJson(address, confirmations)
        Json.stringify(jsRes)
      }
    }
  }

  @Path("/seed/{address}")
  @ApiOperation(value = "Seed", notes = "Export seed value for the {address}", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  def seed = {
    path("seed" / Segment) { case address =>
      jsonRoute {
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
  }

  @Path("/validate/{address}")
  @ApiOperation(value = "Validate", notes = "Check whether address {address} is valid or not", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  def validate = {
    path("validate" / Segment) { case address =>
      jsonRoute {
        val jsRes = Json.obj("address" -> address, "valid" -> Account.isValidAddress(address))
        Json.stringify(jsRes)
      }
    }
  }

  @Path("/")
  @ApiOperation(value = "Addresses", notes = "Get wallet accounts addresses", httpMethod = "GET")
  def root = {
    path("") {
      jsonRoute {
        val addresses = wallet.privateKeyAccounts().map(_.address)
        Json.arr(addresses).toString()
      }
    }
  }

  @Path("/")
  @ApiOperation(value = "Create", notes = "Create a new account in the wallet(if it exists)", httpMethod = "POST")
  def create = {
    path("") {
      jsonRoute({
        val jsRes =
          walletNotExists().getOrElse {
            wallet.generateNewAccount() match {
              case Some(pka) => Json.obj("address" -> pka.address)
              case None => Unknown.json
            }
          }
        Json.stringify(jsRes)
      }, post)
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
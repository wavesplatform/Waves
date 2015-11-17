package scorex.api.http

import java.nio.charset.StandardCharsets
import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import com.wordnik.swagger.annotations._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import scorex.account.{Account, PublicKeyAccount}
import scorex.crypto.{Base58, Curve25519}
import scorex.transaction.state.LagonakiState
import scorex.transaction.state.wallet.{Payment, Wallet}
import spray.http.MediaTypes._

import scala.util.{Failure, Success, Try}


@Api(value = "/addresses", description = "Info about wallet's accounts and other calls about addresses")
case class AddressApiRoute(wallet: Wallet, state: LagonakiState)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  override lazy val route =
    pathPrefix("addresses") {
      root ~ validate ~ seed ~ confirmationBalance ~ balance ~ generatingBalance ~ verify ~ sign ~ deleteAddress ~ create ~ verifyText ~ signText
    }

  @Path("/{address}")
  @ApiOperation(value = "Delete", notes = "Remove the account with address {address} from the wallet", httpMethod = "DELETE")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  def deleteAddress = {
    path(Segment) { case address =>
      jsonRoute({
        val jsRes = walletNotExists(wallet).getOrElse {
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
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with error or json like {\"message\": \"Base58-encoded\",\"publickey\": \"Base58-encoded\", \"signature\": \"Base58-encoded\"}")
  ))
  def sign = {
    path("sign" / Segment) { case address =>
      signPath(address, encode = true)
    }
  }

  @Path("/signText/{address}")
  @ApiOperation(value = "Sign", notes = "Sign a message with a private key associated with {address}", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "message", value = "Message to sign as a plain string", required = true, paramType = "body", dataType = "String"),
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with error or json like {\"message\": \"plain text\",\"publickey\": \"Base58-encoded\", \"signature\": \"Base58-encoded\"}")
  ))
  def signText = {
    path("signText" / Segment) { case address =>
      signPath(address, encode = false)
    }
  }

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
      verifyPath(address, decode = true)
    }
  }

  @Path("/verifyText/{address}")
  @ApiOperation(value = "Verify text", notes = "Check a signature of a message signed by an account", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path"),
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "Message",
      defaultValue = "{\n\t\"message\":\"Plain message\",\n\t\"signature\":\"Base58-encoded signature\",\n\t\"publickey\":\"Base58-encoded public key\"\n}"
    )
  ))
  def verifyText = {
    path("verifyText" / Segment) { case address =>
      verifyPath(address, decode = false)
    }
  }


  @Path("/generatingbalance/{address}")
  @ApiOperation(value = "Generating balance", notes = "Account's generating balance(the same as balance atm)", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  def generatingBalance = {
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
        Json.stringify(balanceJson(address, confirmations))
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
        val jsRes = withPrivateKeyAccount(wallet, address) { account =>
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
          walletNotExists(wallet).getOrElse {
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

  private def signPath(address: String, encode: Boolean) = {
    post {
      respondWithMediaType(`application/json`) {
        entity(as[String]) { message =>
          complete {
            val jsRes = walletNotExists(wallet).getOrElse {
              if (!Account.isValidAddress(address)) {
                InvalidAddress.json
              } else {
                wallet.privateKeyAccount(address) match {
                  case None => WalletAddressNotExists.json
                  case Some(account) =>
                    Try(Curve25519.sign(account, message.getBytes(StandardCharsets.UTF_8))) match {
                      case Success(signature) =>
                        val msg = if (encode) Base58.encode(message.getBytes) else message
                        Json.obj("message" -> msg,
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

  private def verifyPath(address: String, decode: Boolean) = {
    post {
      respondWithMediaType(`application/json`) {
        entity(as[String]) { jsText =>
          complete {
            val parsed = Try(Json.parse(jsText)).getOrElse(WrongJson.json)
            val jsRes = parsed.validate[Message] match {
              case err: JsError =>
                WrongJson.json
              case JsSuccess(m: Message, _) =>
                if (!Account.isValidAddress(address)) {
                  InvalidAddress.json
                } else {
                  //DECODE SIGNATURE
                  val msg: Try[Array[Byte]] = if (decode) Base58.decode(m.message) else Success(m.message.getBytes)
                  (msg, Base58.decode(m.signature), Base58.decode(m.publickey)) match {
                    case (Failure(_), _, _) => InvalidMessage.json
                    case (_, Failure(_), _) => InvalidSignature.json
                    case (_, _, Failure(_)) => InvalidPublicKey.json
                    case (Success(msgBytes), Success(signatureBytes), Success(pubKeyBytes)) =>
                      val account = new PublicKeyAccount(pubKeyBytes)
                      val isValid = account.address == address &&
                        Curve25519.verify(signatureBytes, msgBytes, pubKeyBytes)
                      Json.obj("valid" -> isValid)
                  }
                }
            }
            Json.stringify(jsRes)
          }
        }
      }
    }
  }

  // Workaround to show datatype of post request without using it in another route
  // Related: https://github.com/swagger-api/swagger-core/issues/606
  // Why is this still showing even though it's set to hidden? See https://github.com/martypitt/swagger-springmvc/issues/447
  @ApiOperation(value = "IGNORE", notes = "", hidden = true, httpMethod = "GET", response = classOf[Message])
  protected def messagesModel = Unit

}

case class Message(message: String, signature: String, publickey: String)

object Message {

  implicit val messageReads: Reads[Message] = (
    (JsPath \ "message").read[String] and
      (JsPath \ "signature").read[String] and
      (JsPath \ "publickey").read[String]
    ) (Message.apply _)

}

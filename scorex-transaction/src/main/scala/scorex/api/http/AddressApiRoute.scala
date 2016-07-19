package scorex.api.http

import java.nio.charset.StandardCharsets
import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json._
import scorex.account.{Account, PublicKeyAccount}
import scorex.app.Application
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}

@Path("/addresses")
@Api(value = "/addresses/", description = "Info about wallet's accounts and other calls about addresses")
case class AddressApiRoute(override val application: Application)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  private val wallet = application.wallet
  private val state = application.blockStorage.state
  private implicit val transactionModule = application.transactionModule

  override lazy val route =
    pathPrefix("addresses") {
      validate ~ seed ~ confirmationBalance ~ balance ~ verify ~ sign ~ deleteAddress ~ verifyText ~
        signText ~ seq
    } ~ root ~ create

  @Path("/{address}")
  @ApiOperation(value = "Delete", notes = "Remove the account with address {address} from the wallet", httpMethod = "DELETE")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  def deleteAddress: Route = {
    path(Segment) { case address =>
      withAuth {
        deleteJsonRoute {
          walletNotExists(wallet).getOrElse {
            if (!Account.isValidAddress(address)) {
              InvalidAddress.response
            } else {
              val deleted = wallet.privateKeyAccount(address).exists(account =>
                wallet.deleteAccount(account))
              val json = Json.obj("deleted" -> deleted)

              JsonResponse(json, StatusCodes.OK)
            }
          }
        }
      }
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
  def sign: Route = {
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
  def signText: Route = {
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
      dataType = "scorex.api.http.SignedMessage",
      defaultValue = "{\n\t\"message\":\"Base58-encoded message\",\n\t\"signature\":\"Base58-encoded signature\",\n\t\"publickey\":\"Base58-encoded public key\"\n}"
    )
  ))
  def verify: Route = {
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
      dataType = "scorex.api.http.SignedMessage",
      defaultValue = "{\n\t\"message\":\"Plain message\",\n\t\"signature\":\"Base58-encoded signature\",\n\t\"publickey\":\"Base58-encoded public key\"\n}"
    )
  ))
  def verifyText: Route = {
    path("verifyText" / Segment) { case address =>
      verifyPath(address, decode = false)
    }
  }

  @Path("/balance/{address}")
  @ApiOperation(value = "Balance", notes = "Account's balance", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  def balance: Route = {
    path("balance" / Segment) { case address =>
      getJsonRoute {
        balanceJson(address, 1)
      }
    }
  }

  @Path("/balance/{address}/{confirmations}")
  @ApiOperation(value = "Confirmed balance", notes = "Balance of {address} after {confirmations}", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path"),
    new ApiImplicitParam(name = "confirmations", value = "0", required = true, dataType = "Int", paramType = "path")
  ))
  def confirmationBalance: Route = {
    path("balance" / Segment / IntNumber) { case (address, confirmations) =>
      //todo: confirmations parameter doesn't work atm
      getJsonRoute {
        balanceJson(address, confirmations)
      }
    }
  }

  @Path("/seed/{address}")
  @ApiOperation(value = "Seed", notes = "Export seed value for the {address}", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  def seed: Route = {
    path("seed" / Segment) { case address =>
      withAuth {
        getJsonRoute {
          //TODO CHECK IF WALLET EXISTS
          withPrivateKeyAccount(wallet, address) { account =>
            wallet.exportAccountSeed(account.address) match {
              case None => WalletSeedExportFailed.json
              case Some(seed) => Json.obj("address" -> address, "seed" -> Base58.encode(seed))
            }
          }
        }
      }
    }
  }

  @Path("/validate/{address}")
  @ApiOperation(value = "Validate", notes = "Check whether address {address} is valid or not", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  def validate: Route = {
    path("validate" / Segment) { case address =>
      getJsonRoute {
        val json = Json.obj("address" -> address, "valid" -> Account.isValidAddress(address))
        JsonResponse(json, StatusCodes.OK)
      }
    }
  }

  @Path("/")
  @ApiOperation(value = "Addresses", notes = "Get wallet accounts addresses", httpMethod = "GET")
  def root: Route = {
    path("addresses") {
      getJsonRoute {
        val json = JsArray(wallet.privateKeyAccounts().map(a => JsString(a.address)))
        JsonResponse(json, StatusCodes.OK)
      }
    }
  }

  @Path("/seq/{from}/{to}")
  @ApiOperation(value = "Seq", notes = "Get wallet accounts addresses", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "from", value = "Start address", required = true, dataType = "Int", paramType = "path"),
    new ApiImplicitParam(name = "to", value = "address", required = true, dataType = "Int", paramType = "path")
  ))
  def seq: Route = {
    path("seq" / IntNumber / IntNumber) { case (start, end) =>
      getJsonRoute {
        val json = JsArray(
          wallet.privateKeyAccounts().map(a => JsString(a.address)).slice(start, end)
        )

        JsonResponse(json, StatusCodes.OK)
      }
    }
  }

  @Path("/")
  @ApiOperation(value = "Create", notes = "Create a new account in the wallet(if it exists)", httpMethod = "POST")
  def create: Route = {
    path("addresses") {
      withAuth {
        postJsonRoute {
          walletNotExists(wallet).getOrElse {
            wallet.generateNewAccount() match {
              case Some(pka) => JsonResponse(Json.obj("address" -> pka.address), StatusCodes.OK)
              case None => Unknown.response
            }
          }
        }
      }
    }
  }

  private def balanceJson(address: String, confirmations: Int): JsonResponse = {
    val account = new Account(address)
    if (!Account.isValid(account)) {
      InvalidAddress.response
    } else {
      val json = Json.obj(
        "address" -> account.address,
        "confirmations" -> confirmations,
        "balance" -> state.balanceWithConfirmations(account, confirmations)
      )
      JsonResponse(json, StatusCodes.OK)
    }
  }
  private def signPath(address: String, encode: Boolean) = {
    entity(as[String]) { message =>
      withAuth {
        postJsonRoute {
          walletNotExists(wallet).getOrElse {
            if (!Account.isValidAddress(address)) {
              InvalidAddress.response
            } else {
              wallet.privateKeyAccount(address) match {
                case None => WalletAddressNotExists.response
                case Some(account) =>
                  Try(EllipticCurveImpl.sign(account, message.getBytes(StandardCharsets.UTF_8))) match {
                    case Success(signature) =>
                      val msg = if (encode) Base58.encode(message.getBytes) else message
                      val json = Json.obj("message" -> msg,
                        "publickey" -> Base58.encode(account.publicKey),
                        "signature" -> Base58.encode(signature))
                      JsonResponse(json, StatusCodes.OK)
                    case Failure(t) => JsonResponse(json(t), StatusCodes.InternalServerError)
                  }
              }
            }
          }
        }
      }
    }
  }


  private def verifyPath(address: String, decode: Boolean) = {
    entity(as[String]) { jsText =>
      withAuth {
        postJsonRoute {
          Try(Json.parse(jsText)) match {
            case Success(parsed) => parsed.validate[SignedMessage] match {
              case err: JsError =>
                WrongJson.response
              case JsSuccess(m: SignedMessage, _) =>
                if (!Account.isValidAddress(address)) {
                  InvalidAddress.response
                } else {
                  //DECODE SIGNATURE
                  val msg: Try[Array[Byte]] = if (decode) Base58.decode(m.message) else Success(m.message.getBytes)
                  (msg, Base58.decode(m.signature), Base58.decode(m.publickey)) match {
                    case (Failure(_), _, _) => InvalidMessage.response
                    case (_, Failure(_), _) => InvalidSignature.response
                    case (_, _, Failure(_)) => InvalidPublicKey.response
                    case (Success(msgBytes), Success(signatureBytes), Success(pubKeyBytes)) =>
                      val account = new PublicKeyAccount(pubKeyBytes)
                      val isValid = account.address == address &&
                        EllipticCurveImpl.verify(signatureBytes, msgBytes, pubKeyBytes)
                      JsonResponse(Json.obj("valid" -> isValid), StatusCodes.OK)
                  }
                }
            }
          case Failure(_) => WrongJson.response
        }
      }
    }
  }
}

}

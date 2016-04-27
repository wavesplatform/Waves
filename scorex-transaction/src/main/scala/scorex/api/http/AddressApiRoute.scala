package scorex.api.http

import java.nio.charset.StandardCharsets
import javax.ws.rs.Path

import akka.actor.ActorRefFactory
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

  override lazy val route =
    pathPrefix("addresses") {
      validate ~ seed ~ confirmationBalance ~ balance ~ generatingBalance ~ verify ~ sign ~ deleteAddress ~ verifyText ~
        signText ~ seq
    } ~ root ~ create

  @Path("/{address}")
  @ApiOperation(value = "Delete", notes = "Remove the account with address {address} from the wallet", httpMethod = "DELETE")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  def deleteAddress: Route = {
    path(Segment) { case address =>
      deleteJsonRoute {
        walletNotExists(wallet).getOrElse {
          if (!Account.isValidAddress(address)) {
            InvalidAddress.json
          } else {
            val deleted = wallet.privateKeyAccount(address).exists(account =>
              wallet.deleteAccount(account))
            Json.obj("deleted" -> deleted)
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
      dataType = "SignedMessage",
      defaultValue = "{\n\t\"message\":\"Plain message\",\n\t\"signature\":\"Base58-encoded signature\",\n\t\"publickey\":\"Base58-encoded public key\"\n}"
    )
  ))
  def verifyText: Route = {
    path("verifyText" / Segment) { case address =>
      verifyPath(address, decode = false)
    }
  }


  @Path("/generatingbalance/{address}")
  @ApiOperation(value = "Generating balance", notes = "Account's generating balance(the same as balance atm)", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  def generatingBalance: Route = {
    path("generatingbalance" / Segment) { case address =>
      getJsonRoute {
        if (!Account.isValidAddress(address)) {
          InvalidAddress.json
        } else {
          Json.obj(
            "address" -> address,
            "balance" -> state.generationBalance(address)
          )
        }
      }
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

  @Path("/validate/{address}")
  @ApiOperation(value = "Validate", notes = "Check whether address {address} is valid or not", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "String", paramType = "path")
  ))
  def validate: Route = {
    path("validate" / Segment) { case address =>
      getJsonRoute {
        Json.obj("address" -> address, "valid" -> Account.isValidAddress(address))
      }
    }
  }

  @Path("/")
  @ApiOperation(value = "Addresses", notes = "Get wallet accounts addresses", httpMethod = "GET")
  def root: Route = {
    path("addresses") {
      getJsonRoute {
        JsArray(wallet.privateKeyAccounts().map(a => JsString(a.address)))
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
        JsArray(
          wallet.privateKeyAccounts().map(a => JsString(a.address)).slice(start, end)
        )
      }
    }
  }

  @Path("/")
  @ApiOperation(value = "Create", notes = "Create a new account in the wallet(if it exists)", httpMethod = "POST")
  def create: Route = {
    path("addresses") {
      postJsonRoute({
        walletNotExists(wallet).getOrElse {
          wallet.generateNewAccount() match {
            case Some(pka) => Json.obj("address" -> pka.address)
            case None => Unknown.json
          }
        }
      })
    }
  }

  private def balanceJson(address: String, confirmations: Int) =
    if (!Account.isValidAddress(address)) {
      InvalidAddress.json
    } else {
      Json.obj(
        "address" -> address,
        "confirmations" -> confirmations,
        "balance" -> state.balanceWithConfirmations(address, confirmations)
      )
    }

  private def signPath(address: String, encode: Boolean) = {
    withCors {
      entity(as[String]) { message =>
        complete {
          val jsRes = walletNotExists(wallet).getOrElse {
            if (!Account.isValidAddress(address)) {
              InvalidAddress.json
            } else {
              wallet.privateKeyAccount(address) match {
                case None => WalletAddressNotExists.json
                case Some(account) =>
                  Try(EllipticCurveImpl.sign(account, message.getBytes(StandardCharsets.UTF_8))) match {
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

  private def verifyPath(address: String, decode: Boolean) = {
    withCors {
      entity(as[String]) { jsText =>
        complete {
          val parsed = Try(Json.parse(jsText)).getOrElse(WrongJson.json)
          val jsRes = parsed.validate[SignedMessage] match {
            case err: JsError =>
              WrongJson.json
            case JsSuccess(m: SignedMessage, _) =>
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
                      EllipticCurveImpl.verify(signatureBytes, msgBytes, pubKeyBytes)
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

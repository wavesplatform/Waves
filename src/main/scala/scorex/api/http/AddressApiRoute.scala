package scorex.api.http

import java.nio.charset.StandardCharsets
import javax.ws.rs.Path
import scala.util.{Failure, Success, Try}
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.RestAPISettings
import io.swagger.annotations._
import play.api.libs.json._
import scorex.account.{Account, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.transaction.LagonakiState
import scorex.wallet.Wallet

@Path("/addresses")
@Api(value = "/addresses/", description = "Info about wallet's accounts and other calls about addresses")
case class AddressApiRoute(settings: RestAPISettings, wallet: Wallet, state: LagonakiState) extends ApiRoute {
  val MaxAddressesPerRequest = 1000

  override lazy val route =
    pathPrefix("addresses") {
      validate ~ seed ~ confirmationBalance ~ balance ~ verify ~ sign ~ deleteAddress ~ verifyText ~
        signText ~ seq ~ publicKey
    } ~ root ~ create

  @Path("/{address}")
  @ApiOperation(value = "Delete", notes = "Remove the account with address {address} from the wallet", httpMethod = "DELETE")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  def deleteAddress: Route = path(Segment) { address =>
    (delete & withAuth) {
      if (!Account.isValidAddress(address)) {
        complete(InvalidAddress)
      } else {
        val deleted = wallet.privateKeyAccount(address).exists(account =>
          wallet.deleteAccount(account))
        complete(Json.obj("deleted" -> deleted))
      }
    }
  }

  @Path("/sign/{address}")
  @ApiOperation(value = "Sign", notes = "Sign a message with a private key associated with {address}", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "message", value = "Message to sign as a plain string", required = true, paramType = "body", dataType = "string"),
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with error or json like {\"message\": \"Base58-encoded\",\"publickey\": \"Base58-encoded\", \"signature\": \"Base58-encoded\"}")
  ))
  def sign: Route = {
    path("sign" / Segment) { address =>
      signPath(address, encode = true)
    }
  }

  @Path("/signText/{address}")
  @ApiOperation(value = "Sign", notes = "Sign a message with a private key associated with {address}", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "message", value = "Message to sign as a plain string", required = true, paramType = "body", dataType = "string"),
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with error or json like {\"message\": \"plain text\",\"publickey\": \"Base58-encoded\", \"signature\": \"Base58-encoded\"}")
  ))
  def signText: Route = {
    path("signText" / Segment) { address =>
      signPath(address, encode = false)
    }
  }

  @Path("/verify/{address}")
  @ApiOperation(value = "Verify", notes = "Check a signature of a message signed by an account", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
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
    path("verify" / Segment) { address =>
      verifyPath(address, decode = true)
    }
  }

  @Path("/verifyText/{address}")
  @ApiOperation(value = "Verify text", notes = "Check a signature of a message signed by an account", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.SignedMessage",
      defaultValue = "{\n\t\"message\":\"Plain message\",\n\t\"signature\":\"Base58-encoded signature\",\n\t\"publickey\":\"Base58-encoded public key\"\n}"
    )
  ))
  def verifyText: Route = path("verifyText" / Segment) { address =>
    verifyPath(address, decode = false)
  }

  @Path("/balance/{address}")
  @ApiOperation(value = "Balance", notes = "Account's balance", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  def balance: Route = (path("balance" / Segment) & get) { address =>
    complete(balanceJson(address, 0))
  }

  @Path("/balance/{address}/{confirmations}")
  @ApiOperation(value = "Confirmed balance", notes = "Balance of {address} after {confirmations}", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "confirmations", value = "0", required = true, dataType = "integer", paramType = "path")
  ))
  def confirmationBalance: Route = {
    (path("balance" / Segment / IntNumber) & get) { case (address, confirmations) =>
      //todo: confirmations parameter doesn't work atm
      complete(balanceJson(address, confirmations))
    }
  }

  @Path("/seed/{address}")
  @ApiOperation(value = "Seed", notes = "Export seed value for the {address}", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  def seed: Route = {
    (path("seed" / Segment) & get & withAuth) { address =>
      if (!Account.isValidAddress(address)) {
        complete(InvalidAddress)
      } else {
        wallet.privateKeyAccount(address) match {
          case None => complete(WalletAddressNotExists)
          case Some(account) =>
            wallet.exportAccountSeed(account.address) match {
              case None => complete(WalletSeedExportFailed)
              case Some(seed) => complete(Json.obj("address" -> address, "seed" -> Base58.encode(seed)))
            }
        }
      }
    }
  }

  @Path("/validate/{address}")
  @ApiOperation(value = "Validate", notes = "Check whether address {address} is valid or not", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  def validate: Route = (path("validate" / Segment) & get) { address =>
    complete(Json.obj("address" -> address, "valid" -> Account.isValidAddress(address)))
  }

  @Path("/")
  @ApiOperation(value = "Addresses", notes = "Get wallet accounts addresses", httpMethod = "GET")
  def root: Route = (path("addresses") & get) {
    val accounts = wallet.privateKeyAccounts()
    val json = JsArray(accounts.map(a => JsString(a.address)))
    complete(json)
  }

  @Path("/seq/{from}/{to}")
  @ApiOperation(value = "Seq", notes = "Get wallet accounts addresses", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "from", value = "Start address", required = true, dataType = "integer", paramType = "path"),
    new ApiImplicitParam(name = "to", value = "address", required = true, dataType = "integer", paramType = "path")
  ))
  def seq: Route = {
    (path("seq" / IntNumber / IntNumber) & get) { case (start, end) =>
      if (start >= 0 && end >= 0 && start - end < MaxAddressesPerRequest) {
        val json = JsArray(
          wallet.privateKeyAccounts().map(a => JsString(a.address)).slice(start, end)
        )

        complete(json)
      } else complete(TooBigArrayAllocation)
    }
  }

  @Path("/")
  @ApiOperation(value = "Create", notes = "Create a new account in the wallet(if it exists)", httpMethod = "POST")
  def create: Route = (path("addresses") & post) {
    withAuth {
      wallet.generateNewAccount() match {
        case Some(pka) => complete(Json.obj("address" -> pka.address))
        case None => complete(Unknown)
      }
    }
  }

  private def balanceJson(address: String, confirmations: Int): ToResponseMarshallable = {
    val account = new Account(address)
    if (!Account.isValid(account)) {
      InvalidAddress
    } else {
      Json.obj(
        "address" -> account.address,
        "confirmations" -> confirmations,
        "balance" -> state.balanceWithConfirmations(account, confirmations))
    }
  }

  private def signPath(address: String, encode: Boolean) = {
    (post & entity(as[String])) { message =>
      withAuth {
        if (!Account.isValidAddress(address)) {
          complete(InvalidAddress)
        } else {
          wallet.privateKeyAccount(address) match {
            case None => complete(WalletAddressNotExists)
            case Some(account) =>
              val messageBytes = message.getBytes(StandardCharsets.UTF_8)
              Try(EllipticCurveImpl.sign(account, messageBytes)) match {
                case Success(signature) =>
                  val msg = if (encode) Base58.encode(messageBytes) else message
                  val json = Json.obj("message" -> msg,
                    "publicKey" -> Base58.encode(account.publicKey),
                    "signature" -> Base58.encode(signature))
                  complete(json)
                case Failure(t) => complete(StatusCodes.InternalServerError)
              }
          }
        }
      }
    }
  }

  private def verifyPath(address: String, decode: Boolean) = withAuth {
    json[SignedMessage] { m =>
      if (!Account.isValidAddress(address)) {
        InvalidAddress
      } else {
        //DECODE SIGNATURE
        val msg: Try[Array[Byte]] = if (decode) Base58.decode(m.message) else Success(m.message.getBytes)
        verifySigned(msg, m.signature, m.publickey, address)
      }
    }
  }

  private def verifySigned(msg: Try[Array[Byte]], signature: String, publicKey: String, address: String) = {
    (msg, Base58.decode(signature), Base58.decode(publicKey)) match {
      case (Success(msgBytes), Success(signatureBytes), Success(pubKeyBytes)) =>
        val account = new PublicKeyAccount(pubKeyBytes)
        val isValid = account.address == address && EllipticCurveImpl.verify(signatureBytes, msgBytes, pubKeyBytes)
        Right(Json.obj("valid" -> isValid))
      case _ => Left(InvalidMessage)
    }
  }

  @Path("/publicKey/{publicKey}")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "publicKey", value = "Public key Base58-encoded", required = true,
      paramType = "path", dataType = "string")
  ))
  @ApiOperation(value = "Address from Public Key", notes = "Generate a address from public key", httpMethod = "GET")
  def publicKey: Route = (path("publicKey" / Segment) & get) { publicKey =>
    Base58.decode(publicKey) match {
      case Success(pubKeyBytes) => {
        val account = Account.fromPublicKey(pubKeyBytes)
        complete(Json.obj("address" -> account.address))
      }
      case Failure(e) => complete(InvalidPublicKey)
    }
  }
}

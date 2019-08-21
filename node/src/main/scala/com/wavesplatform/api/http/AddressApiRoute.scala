package com.wavesplatform.api.http

import java.nio.charset.StandardCharsets
import java.util.regex.Pattern

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.common.CommonAccountApi
import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.crypto
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.TransactionFactory
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import io.swagger.annotations._
import javax.ws.rs.Path
import monix.execution.Scheduler
import play.api.libs.json._

import scala.util.{Failure, Success, Try}

@Path("/addresses")
@Api(value = "/addresses/")
case class AddressApiRoute(settings: RestAPISettings, wallet: Wallet, blockchain: Blockchain, utxPoolSynchronizer: UtxPoolSynchronizer, time: Time)
    extends ApiRoute
    with BroadcastRoute
    with AuthRoute {

  import AddressApiRoute._

  private[this] val commonAccountApi = new CommonAccountApi(blockchain)
  val MaxAddressesPerRequest         = 1000

  override lazy val route =
    pathPrefix("addresses") {
      validate ~ seed ~ balanceWithConfirmations ~ balanceDetails ~ balance ~ balanceWithConfirmations ~ verify ~ sign ~ deleteAddress ~ verifyText ~
        signText ~ seq ~ publicKey ~ effectiveBalance ~ effectiveBalanceWithConfirmations ~ getData ~ getDataItem ~ postData ~ scriptInfo
    } ~ root ~ create

  @Path("/scriptInfo/{address}")
  @ApiOperation(value = "Details for account", notes = "Account's script", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    )
  )
  def scriptInfo: Route = (path("scriptInfo" / Segment) & get) { address =>
    complete(
      Address
        .fromString(address)
        .map(addressScriptInfoJson)
        .map(ToResponseMarshallable(_))
    )
  }
  @Path("/{address}")
  @ApiOperation(value = "Delete", notes = "Remove the account with address {address} from the wallet", httpMethod = "DELETE")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    )
  )
  def deleteAddress: Route = path(Segment) { address =>
    (delete & withAuth) {
      if (Address.fromString(address).isLeft) {
        complete(InvalidAddress)
      } else {
        val deleted = wallet.findPrivateKey(address).exists(account => wallet.deleteAccount(account))
        complete(Json.obj("deleted" -> deleted))
      }
    }
  }

  @Path("/sign/{address}")
  @ApiOperation(value = "Sign", notes = "Sign a message with a private key associated with {address}", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "message", value = "Message to sign as a plain string", required = true, paramType = "body", dataType = "string"),
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    )
  )
  @ApiResponses(
    Array(
      new ApiResponse(
        code = 200,
        message =
          "Json with error or json like {\"message\": \"Base58-encoded\",\"publickey\": \"Base58-encoded\", \"signature\": \"Base58-encoded\"}"
      )
    )
  )
  def sign: Route = {
    path("sign" / Segment) { address =>
      signPath(address, encode = true)
    }
  }

  @Path("/signText/{address}")
  @ApiOperation(value = "Sign", notes = "Sign a message with a private key associated with {address}", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "message", value = "Message to sign as a plain string", required = true, paramType = "body", dataType = "string"),
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    )
  )
  @ApiResponses(
    Array(
      new ApiResponse(
        code = 200,
        message = "Json with error or json like {\"message\": \"plain text\",\"publickey\": \"Base58-encoded\", \"signature\": \"Base58-encoded\"}"
      )
    )
  )
  def signText: Route = {
    path("signText" / Segment) { address =>
      signPath(address, encode = false)
    }
  }

  @Path("/verify/{address}")
  @ApiOperation(value = "Verify", notes = "Check a signature of a message signed by an account", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.api.http.SignedMessage",
        defaultValue =
          "{\n\t\"message\":\"Base58-encoded message\",\n\t\"signature\":\"Base58-encoded signature\",\n\t\"publickey\":\"Base58-encoded public key\"\n}"
      )
    )
  )
  def verify: Route = path("verify" / Segment) { address =>
    verifyPath(address, decode = true)
  }

  @Path("/verifyText/{address}")
  @ApiOperation(value = "Verify text", notes = "Check a signature of a message signed by an account", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.api.http.SignedMessage",
        defaultValue =
          "{\n\t\"message\":\"Plain message\",\n\t\"signature\":\"Base58-encoded signature\",\n\t\"publickey\":\"Base58-encoded public key\"\n}"
      )
    )
  )
  def verifyText: Route = path("verifyText" / Segment) { address =>
    verifyPath(address, decode = false)
  }

  @Path("/balance/{address}")
  @ApiOperation(value = "Balance", notes = "Account's balance", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    )
  )
  def balance: Route = (path("balance" / Segment) & get) { address =>
    complete(balanceJson(address))
  }

  @Path("/balance/details/{address}")
  @ApiOperation(value = "Details for balance", notes = "Account's balances", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    )
  )
  def balanceDetails: Route = (path("balance" / "details" / Segment) & get) { address =>
    complete(
      Address
        .fromString(address)
        .right
        .map(acc => {
          ToResponseMarshallable(balancesDetailsJson(acc))
        })
        .getOrElse(InvalidAddress)
    )
  }

  @Path("/balance/{address}/{confirmations}")
  @ApiOperation(value = "Confirmed balance", notes = "Balance of {address} after {confirmations}", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "confirmations", value = "0", required = true, dataType = "integer", paramType = "path")
    )
  )
  def balanceWithConfirmations: Route = {
    (path("balance" / Segment / IntNumber) & get) {
      case (address, confirmations) =>
        complete(balanceJson(address, confirmations))
    }
  }

  @Path("/effectiveBalance/{address}")
  @ApiOperation(value = "Balance", notes = "Account's balance", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    )
  )
  def effectiveBalance: Route = {
    path("effectiveBalance" / Segment) { address =>
      complete(effectiveBalanceJson(address, 0))
    }
  }

  @Path("/effectiveBalance/{address}/{confirmations}")
  @ApiOperation(value = "Confirmed balance", notes = "Balance of {address} after {confirmations}", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "confirmations", value = "0", required = true, dataType = "integer", paramType = "path")
    )
  )
  def effectiveBalanceWithConfirmations: Route = {
    path("effectiveBalance" / Segment / IntNumber) {
      case (address, confirmations) =>
        complete(
          effectiveBalanceJson(address, confirmations)
        )
    }
  }

  @Path("/seed/{address}")
  @ApiOperation(value = "Seed", notes = "Export seed value for the {address}", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    )
  )
  def seed: Route = {
    (path("seed" / Segment) & get & withAuth) { address =>
      complete(for {
        pk   <- wallet.findPrivateKey(address)
        seed <- wallet.exportAccountSeed(pk)
      } yield Json.obj("address" -> address, "seed" -> Base58.encode(seed)))
    }
  }

  @Path("/validate/{address}")
  @ApiOperation(value = "Validate", notes = "Check whether address {address} is valid or not", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    )
  )
  def validate: Route = (path("validate" / Segment) & get) { address =>
    complete(Validity(address, Address.fromString(address).isRight))
  }

  @Path("/data")
  @ApiOperation(value = "Post Data to Blockchain", httpMethod = "POST", produces = "application/json", consumes = "application/json")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.api.http.DataRequest",
        defaultValue =
          "{\n\t\"version\": 1,\n\t\"sender\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n\t\"fee\": 100000,\n\t\"data\": [{\"key\":\"intValue\", \"type\":\"integer\", \"value\":17},{\"key\":\"stringValue\", \"type\":\"string\", \"value\":\"seventeen\"},{\"key\":\"boolValue\", \"type\":\"boolean\", \"value\":false},{\"key\":\"binaryArray\", \"type\":\"binary\", \"value\":\"EQ==\"}]\n}"
      )
    )
  )
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def postData: Route = (path("data") & withAuth) {
    broadcast[DataRequest](data => TransactionFactory.data(data, wallet, time))
  }

  @Path("/data/{address}")
  @ApiOperation(value = "Complete Data", notes = "Read all data posted by an account", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "matches",
        value = "URL encoded (percent-encoded) regular expression to filter keys (https://www.tutorialspoint.com/scala/scala_regular_expressions.htm)",
        required = false,
        dataType = "string",
        paramType = "query"
      )
    )
  )
  def getData: Route =
    extractScheduler(
      implicit sc =>
        (path("data" / Segment) & parameter('matches.?) & get) { (address, maybeRegex) =>
          maybeRegex match {
            case None => complete(accountData(address))
            case Some(regex) =>
              complete(
                Try(regex.r)
                  .fold(
                    _ => ApiError.fromValidationError(GenericError(s"Cannot compile regex")),
                    r => accountData(address, r.pattern)
                  )
              )

          }
        }
    )

  @Path("/data/{address}/{key}")
  @ApiOperation(value = "Data by Key", notes = "Read data associated with an account and a key", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "key", value = "Data key", required = true, dataType = "string", paramType = "path")
    )
  )
  def getDataItem: Route = (path("data" / Segment / Segment) & get) {
    case (address, key) =>
      complete(accountData(address, key))
  }

  @Path("/")
  @ApiOperation(value = "Addresses", notes = "Get wallet accounts addresses", httpMethod = "GET")
  def root: Route = (path("addresses") & get) {
    val accounts = wallet.privateKeyAccounts
    val json     = JsArray(accounts.map(a => JsString(a.address)))
    complete(json)
  }

  @Path("/seq/{from}/{to}")
  @ApiOperation(value = "Seq", notes = "Get wallet accounts addresses", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "from", value = "Start address", required = true, dataType = "integer", paramType = "path"),
      new ApiImplicitParam(name = "to", value = "address", required = true, dataType = "integer", paramType = "path")
    )
  )
  def seq: Route = {
    (path("seq" / IntNumber / IntNumber) & get) {
      case (start, end) =>
        if (start >= 0 && end >= 0 && start - end < MaxAddressesPerRequest) {
          val json = JsArray(
            wallet.privateKeyAccounts.map(a => JsString(a.address)).slice(start, end)
          )

          complete(json)
        } else complete(TooBigArrayAllocation)
    }
  }

  @Path("/")
  @ApiOperation(value = "Create", notes = "Create a new account in the wallet(if it exists)", httpMethod = "POST")
  def create: Route = (path("addresses") & post & withAuth) {
    wallet.generateNewAccount() match {
      case Some(pka) => complete(Json.obj("address" -> pka.address))
      case None      => complete(Unknown)
    }
  }

  private def balanceJson(address: String, confirmations: Int): ToResponseMarshallable = {
    Address
      .fromString(address)
      .right
      .map(
        acc =>
          ToResponseMarshallable(
            Balance(
              acc.address,
              confirmations,
              commonAccountApi.balance(acc, confirmations)
            )
          )
      )
      .getOrElse(InvalidAddress)
  }

  private def balanceJson(address: String): ToResponseMarshallable = {
    Address
      .fromString(address)
      .right
      .map(acc => ToResponseMarshallable(Balance(acc.address, 0, commonAccountApi.balance(acc))))
      .getOrElse(InvalidAddress)
  }

  private def balancesDetailsJson(account: Address): BalanceDetails = {
    val details = commonAccountApi.balanceDetails(account)
    import details._
    BalanceDetails(account.address, regular, generating, available, effective)
  }

  private def addressScriptInfoJson(account: Address): AddressScriptInfo = {
    val CommonAccountApi.AddressScriptInfo(script, scriptText, complexity, extraFee) = commonAccountApi.script(account)
    AddressScriptInfo(account.address, script.map(_.base64), scriptText, complexity, extraFee)
  }

  private def effectiveBalanceJson(address: String, confirmations: Int): ToResponseMarshallable = {
    Address
      .fromString(address)
      .right
      .map(acc => ToResponseMarshallable(Balance(acc.address, confirmations, commonAccountApi.effectiveBalance(acc, confirmations))))
      .getOrElse(InvalidAddress)
  }

  private def accountData(address: String)(implicit sc: Scheduler): ToResponseMarshallable = {
    Address
      .fromString(address)
      .map { acc =>
        ToResponseMarshallable(commonAccountApi.dataStream(acc).toListL.runAsyncLogErr.map(_.sortBy(_.key)))
      }
      .getOrElse(InvalidAddress)
  }

  private def accountData(address: String, regex: Pattern)(implicit sc: Scheduler): ToResponseMarshallable = {
    Address
      .fromString(address)
      .map { addr =>
        val result: ToResponseMarshallable = commonAccountApi
          .dataStream(addr, k => regex.matcher(k).matches())
          .toListL
          .runAsyncLogErr
          .map(_.sortBy(_.key))

        result
      }
      .getOrElse(InvalidAddress)
  }

  private def accountData(address: String, key: String): ToResponseMarshallable = {
    val result = for {
      addr  <- Address.fromString(address).left.map(_ => InvalidAddress)
      value <- commonAccountApi.data(addr, key).toRight(DataKeyDoesNotExist)
    } yield value
    ToResponseMarshallable(result)
  }

  private def signPath(address: String, encode: Boolean) = (post & entity(as[String])) { message =>
    withAuth {
      val res = wallet
        .findPrivateKey(address)
        .map(pk => {
          val messageBytes = message.getBytes(StandardCharsets.UTF_8)
          val signature    = crypto.sign(pk, messageBytes)
          val msg          = if (encode) Base58.encode(messageBytes) else message
          Signed(msg, Base58.encode(pk.publicKey), Base58.encode(signature))
        })
      complete(res)
    }
  }

  private def verifyPath(address: String, decode: Boolean): Route = withAuth {
    jsonPost[SignedMessage] { m =>
      if (Address.fromString(address).isLeft) {
        InvalidAddress
      } else {
        //DECODE SIGNATURE
        val msg: Try[Array[Byte]] =
          if (decode) if (m.message.startsWith("base64:")) Base64.tryDecode(m.message) else Base58.tryDecodeWithLimit(m.message, 2048)
          else Success(m.message.getBytes("UTF-8"))
        verifySigned(msg, m.signature, m.publickey, address)
      }
    }
  }

  private def verifySigned(msg: Try[Array[Byte]], signature: String, publicKey: String, address: String) = {
    (msg, Base58.tryDecodeWithLimit(signature), Base58.tryDecodeWithLimit(publicKey)) match {
      case (Success(msgBytes), Success(signatureBytes), Success(pubKeyBytes)) =>
        val account = PublicKey(pubKeyBytes)
        val isValid = account.address == address && crypto.verify(signatureBytes, msgBytes, PublicKey(pubKeyBytes))
        Right(Json.obj("valid" -> isValid))
      case _ => Left(InvalidMessage)
    }
  }

  @Path("/publicKey/{publicKey}")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "publicKey", value = "Public key Base58-encoded", required = true, paramType = "path", dataType = "string")
    )
  )
  @ApiOperation(value = "Address from Public Key", notes = "Generate a address from public key", httpMethod = "GET")
  def publicKey: Route = (path("publicKey" / Segment) & get) { publicKey =>
    Base58.tryDecodeWithLimit(publicKey) match {
      case Success(pubKeyBytes) =>
        val account = Address.fromPublicKey(PublicKey(pubKeyBytes))
        complete(Json.obj("address" -> account.address))
      case Failure(_) => complete(InvalidPublicKey)
    }
  }
}

object AddressApiRoute {

  case class Signed(message: String, publicKey: String, signature: String)

  implicit val signedFormat: Format[Signed] = Json.format

  case class Balance(address: String, confirmations: Int, balance: Long)

  implicit val balanceFormat: Format[Balance] = Json.format

  case class BalanceDetails(address: String, regular: Long, generating: Long, available: Long, effective: Long)

  implicit val balanceDetailsFormat: Format[BalanceDetails] = Json.format

  case class Validity(address: String, valid: Boolean)

  implicit val validityFormat: Format[Validity] = Json.format

  case class AddressScriptInfo(address: String, script: Option[String], scriptText: Option[String], complexity: Long, extraFee: Long)

  implicit val accountScriptInfoFormat: Format[AddressScriptInfo] = Json.format
}

package com.wavesplatform.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.common.CommonAccountsApi
import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.api.http.requests.DataRequest
import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.crypto
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.lang.contract.meta.Dic
import com.wavesplatform.lang.{Global, ValidationError}
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.protobuf.api
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.transaction.TransactionFactory
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.{Time, _}
import com.wavesplatform.wallet.Wallet
import io.swagger.annotations._
import javax.ws.rs.Path
import monix.execution.Scheduler
import play.api.libs.json._

import scala.util.{Success, Try}

@Path("/addresses")
@Api(value = "/addresses/")
case class AddressApiRoute(
    settings: RestAPISettings,
    wallet: Wallet,
    blockchain: Blockchain,
    utxPoolSynchronizer: UtxPoolSynchronizer,
    time: Time,
    commonAccountsApi: CommonAccountsApi
) extends ApiRoute
    with BroadcastRoute
    with AuthRoute
    with AutoParamsDirective {

  import AddressApiRoute._

  val MaxAddressesPerRequest = 1000

  override lazy val route =
    pathPrefix("addresses") {
      balanceDetails ~ validate ~ seed ~ balanceWithConfirmations ~ balance ~ balanceWithConfirmations ~ verify ~ sign ~ deleteAddress ~ verifyText ~
        signText ~ seq ~ publicKey ~ effectiveBalance ~ effectiveBalanceWithConfirmations ~ getData ~ getDataItem ~ postData ~ scriptInfo ~ scriptMeta
    } ~ root ~ create

  @Path("/scriptInfo/{address}")
  @ApiOperation(value = "Details for account", notes = "Account's script", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    )
  )
  def scriptInfo: Route = (path("scriptInfo" / AddrSegment) & get) { address =>
    val script = blockchain.accountScript(address)
    complete(
      Json.obj(
        "address"            -> address.stringRepr,
        "script"             -> script.map(_.script.bytes().base64),
        "scriptText"         -> script.map(_.script.expr.toString),
        "complexity"         -> script.fold(0L)(_.verifierComplexity),
        "callableComplexity" -> script.fold[JsValue](JsNull)(m => Json.toJson(m.callableComplexity)),
        "extraFee"           -> (if (script.isEmpty) 0L else FeeValidation.ScriptExtraFee)
      )
    )
  }

  @Path("/scriptInfo/{address}/meta")
  @ApiOperation(value = "Meta by address", notes = "Account's script meta", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    )
  )
  def scriptMeta: Route = (path("scriptInfo" / AddrSegment / "meta") & get) { address =>
    complete(scriptMetaJson(address))
  }

  @Path("/{address}")
  @ApiOperation(value = "Delete", notes = "Remove the account with address {address} from the wallet", httpMethod = "DELETE")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    )
  )
  def deleteAddress: Route = (delete & withAuth & path(AddrSegment)) { address =>
    val deleted = wallet.privateKeyAccount(address).exists(account => wallet.deleteAccount(account))
    complete(Json.obj("deleted" -> deleted))
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
    path("sign" / AddrSegment) { address =>
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
    path("signText" / AddrSegment) { address =>
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
  def verify: Route = path("verify" / AddrSegment) { address =>
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
  def verifyText: Route = path("verifyText" / AddrSegment) { address =>
    verifyPath(address, decode = false)
  }

  @Path("/balance/{address}")
  @ApiOperation(value = "Balance", notes = "Account's balance", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    )
  )
  def balance: Route = (path("balance" / AddrSegment) & get) { address =>
    complete(balanceJson(address))
  }

  @Path("/balance/details/{address}")
  @ApiOperation(value = "Details for balance", notes = "Account's balances", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    )
  )
  def balanceDetails: Route = (path("balance" / "details" / AddrSegment) & get) { address =>
    val details = commonAccountsApi.balanceDetails(address)
    import details._
    complete(
      Json.obj("address" -> address.stringRepr, "regular" -> regular, "generating" -> generating, "available" -> available, "effective" -> effective)
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
    (path("balance" / AddrSegment / IntNumber) & get) {
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
    path("effectiveBalance" / AddrSegment) { address =>
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
    path("effectiveBalance" / AddrSegment / IntNumber) { (address, confirmations) =>
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
  def validate: Route = (path("validate" / Segment) & get) { addressBytes =>
    complete(Json.obj("address" -> addressBytes, "valid" -> Address.fromString(addressBytes).isRight))
  }

  // TODO: Remove from API
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
      ),
      new ApiImplicitParam(
        name = "key",
        value = "Exact keys to query",
        required = false,
        dataType = "string",
        paramType = "query",
        allowMultiple = true
      )
    )
  )
  def getData: Route =
    extractScheduler(
      implicit sc =>
        path("data" / AddrSegment) { address =>
          protobufEntity(api.DataRequest) { request =>
            if (request.matches.nonEmpty)
              complete(
                Try(request.matches.r)
                  .fold(
                    { e =>
                      log.error(s"Error compiling regex ${request.matches}", e)
                      ApiError.fromValidationError(GenericError(s"Cannot compile regex"))
                    },
                    _ => accountData(address, request.matches)
                  )
              )
            else complete(accountDataList(address, request.keys: _*))
          } ~ get {
            complete(accountData(address))
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
  def getDataItem: Route = (path("data" / AddrSegment / Segment) & get) {
    case (address, key) =>
      complete(accountDataEntry(address, key))
  }

  @Path("/")
  @ApiOperation(value = "Addresses", notes = "Get wallet accounts addresses", httpMethod = "GET")
  def root: Route = (path("addresses") & get) {
    val accounts = wallet.privateKeyAccounts
    val json     = JsArray(accounts.map(a => JsString(a.stringRepr)))
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
            wallet.privateKeyAccounts.map(a => JsString(a.stringRepr)).slice(start, end)
          )

          complete(json)
        } else complete(TooBigArrayAllocation)
    }
  }

  @Path("/")
  @ApiOperation(value = "Create", notes = "Create a new account in the wallet(if it exists)", httpMethod = "POST")
  def create: Route = (path("addresses") & post & withAuth) {
    wallet.generateNewAccount() match {
      case Some(pka) => complete(Json.obj("address" -> pka.stringRepr))
      case None      => complete(Unknown)
    }
  }

  private def balanceJson(acc: Address, confirmations: Int) =
    Balance(acc.stringRepr, confirmations, commonAccountsApi.balance(acc, confirmations))

  private def balanceJson(acc: Address) = Balance(acc.stringRepr, 0, commonAccountsApi.balance(acc))

  private def scriptMetaJson(account: Address): Either[ValidationError.ScriptParseError, AccountScriptMeta] = {
    import cats.implicits._
    val accountScript = blockchain.accountScript(account)

    accountScript
      .map(_.script)
      .traverse(Global.dAppFuncTypes)
      .map(AccountScriptMeta(account.stringRepr, _))
  }

  private def effectiveBalanceJson(acc: Address, confirmations: Int) = {
    Balance(acc.stringRepr, confirmations, commonAccountsApi.effectiveBalance(acc, confirmations))
  }

  private def accountData(address: Address)(implicit sc: Scheduler) =
    commonAccountsApi.dataStream(address, None).toListL.runAsyncLogErr.map(_.sortBy(_.key))

  private def accountData(addr: Address, regex: String)(implicit sc: Scheduler): ToResponseMarshallable =
    commonAccountsApi
      .dataStream(addr, Some(regex))
      .toListL
      .runAsyncLogErr
      .map(_.sortBy(_.key))

  private def accountDataEntry(address: Address, key: String): ToResponseMarshallable =
    commonAccountsApi.data(address, key).toRight(DataKeyDoesNotExist)

  private def accountDataList(address: Address, keys: String*): ToResponseMarshallable =
    keys.flatMap(commonAccountsApi.data(address, _))

  private def signPath(address: Address, encode: Boolean): Route = (post & entity(as[String])) { message =>
    withAuth {
      val res = wallet
        .privateKeyAccount(address)
        .map(pk => {
          val messageBytes = message.utf8Bytes
          val signature    = crypto.sign(pk, messageBytes)
          val msg          = if (encode) Base58.encode(messageBytes) else message
          Signed(msg, Base58.encode(pk.publicKey), Base58.encode(signature))
        })
      complete(res)
    }
  }

  private def verifyPath(address: Address, decode: Boolean): Route = withAuth {
    jsonPost[SignedMessage] { m =>
      val msg: Try[Array[Byte]] =
        if (decode) if (m.message.startsWith("base64:")) Base64.tryDecode(m.message) else Base58.tryDecodeWithLimit(m.message, 2048)
        else Success(m.message.utf8Bytes)
      verifySigned(msg, m.signature, m.publickey, address)
    }
  }

  private def verifySigned(msg: Try[Array[Byte]], signature: String, publicKey: String, address: Address) = {
    (msg, Base58.tryDecodeWithLimit(signature), Base58.tryDecodeWithLimit(publicKey)) match {
      case (Success(msgBytes), Success(signatureBytes), Success(pubKeyBytes)) =>
        val account = PublicKey(pubKeyBytes)
        val isValid = account.toAddress == address && crypto.verify(signatureBytes, msgBytes, PublicKey(pubKeyBytes))
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
  def publicKey: Route = (path("publicKey" / PublicKeySegment) & get) { publicKey =>
    complete(Json.obj("address" -> Address.fromPublicKey(publicKey).stringRepr))
  }
}

object AddressApiRoute {
  case class Signed(message: String, publicKey: String, signature: String)

  implicit val signedFormat: Format[Signed] = Json.format

  case class Balance(address: String, confirmations: Int, balance: Long)

  implicit val balanceFormat: Format[Balance] = Json.format

  case class AccountScriptMeta(address: String, meta: Option[Dic])
  implicit lazy val accountScriptMetaWrites: Writes[AccountScriptMeta] = Json.writes[AccountScriptMeta]
  implicit lazy val dicFormat: Writes[Dic]                             = metaConverter.foldRoot
}

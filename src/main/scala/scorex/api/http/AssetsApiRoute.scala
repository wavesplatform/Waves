package scorex.api.http

import javax.ws.rs.Path
import scala.util.{Failure, Success}
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{RejectionHandler, Route, ValidationRejection}
import com.wavesplatform.http.PlayJsonException
import com.wavesplatform.http.PlayJsonSupport._
import com.wavesplatform.settings.RestAPISettings
import io.swagger.annotations._
import play.api.libs.json._
import scorex.account.Account
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.Order
import scorex.transaction.assets.exchange.OrderJson._
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.state.wallet._
import scorex.transaction.{AssetAcc, TransactionOperations}
import scorex.wallet.Wallet

@Path("/assets")
@Api(value = "assets")
case class AssetsApiRoute(settings: RestAPISettings, wallet: Wallet, state: StoredState, transactionModule: TransactionOperations) extends ApiRoute with CommonTransactionApiFunctions {
  val MaxAddressesPerRequest = 1000

  private def processRequest[A: Reads](pathMatcher: String, f: A => ToResponseMarshallable) =
    (path(pathMatcher) & post & withAuth) {
      process[A](f)
    }

  private val defaultRejectionHandler = RejectionHandler.newBuilder().handle {
    case ValidationRejection(_, Some(PlayJsonException(cause, errors))) => complete(WrongJson(cause, errors))
  }.result()

  override lazy val route =
    (handleRejections(defaultRejectionHandler) & pathPrefix("assets")) {
      balance ~ balances ~ issue ~ reissue ~ burnRoute ~ transfer ~ signOrder ~ balanceDistribution
    }

  @Path("/balance/{address}/{assetId}")
  @ApiOperation(value = "Asset's balance", notes = "Account's balance by given asset", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "assetId", value = "Asset ID", required = true, dataType = "string", paramType = "path")
  ))
  def balance: Route = {
    path("balance" / Segment / Segment) { case (address, assetId) =>
      getJsonRoute {
        balanceJson(address, assetId)
      }
    }
  }

  @Path("/{assetId}/distribution")
  @ApiOperation(value = "Asset balance distribution", notes = "Asset balance distribution by account", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "assetId", value = "Asset ID", required = true, dataType = "string", paramType = "path")
  ))
  def balanceDistribution: Route = {
    path(Segment / "distribution") { assetId =>
      getJsonRoute {
        Base58.decode(assetId) match {
          case Success(byteArray) => JsonResponse(Json.toJson(state.assetDistribution(byteArray)), StatusCodes.OK)
          case Failure(e) => ApiError.fromValidationError(scorex.transaction.ValidationError.CustomValidationError("Must be base58-encoded assetId")).response
        }
      }
    }
  }


  @Path("/balance/{address}")
  @ApiOperation(value = "Account's balance", notes = "Account's balances for all assets", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  def balances: Route = {
    path("balance" / Segment) { address =>
      getJsonRoute {
        balanceJson(address)
      }
    }
  }

  @Path("/transfer")
  @ApiOperation(value = "Transfer asset",
    notes = "Transfer asset to new address",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.state.wallet.TransferRequest",
      defaultValue = "{\"sender\":\"3Mn6xomsZZepJj1GL1QaW6CaCJAq8B3oPef\",\"recipient\":\"3Mciuup51AxRrpSz7XhutnQYTkNT9691HAk\",\"assetId\":null,\"amount\":5813874260609385500,\"feeAssetId\":\"3Z7T9SwMbcBuZgcn3mGu7MMp619CTgSWBT7wvEkPwYXGnoYzLeTyh3EqZu1ibUhbUHAsGK5tdv9vJL9pk4fzv9Gc\",\"fee\":1579331567487095949,\"timestamp\":4231642878298810008}"
    )
  ))
  def transfer: Route =
    processRequest("transfer", (t: TransferRequest) => transactionModule.transferAsset(t, wallet))

  @Path("/issue")
  @ApiOperation(value = "Issue Asset",
    notes = "Issue new Asset",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.state.wallet.IssueRequest",
      defaultValue = "{\"sender\":\"string\",\"name\":\"str\",\"description\":\"string\",\"quantity\":100000,\"decimals\":7,\"reissuable\":false,\"fee\":100000000}"
    )
  ))
  def issue: Route =
    processRequest("issue", (i: IssueRequest) => transactionModule.issueAsset(i, wallet))

  @Path("/reissue")
  @ApiOperation(value = "Issue Asset",
    notes = "Reissue Asset",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.state.wallet.ReissueRequest",
      defaultValue = "{\"sender\":\"string\",\"assetId\":\"Base58\",\"quantity\":100000,\"reissuable\":false,\"fee\":1}"
    )
  ))
  def reissue: Route =
    processRequest("reissue", (r: ReissueRequest) => transactionModule.reissueAsset(r, wallet))

  @Path("/burn")
  @ApiOperation(value = "Burn Asset",
    notes = "Burn some of your assets",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.state.wallet.BurnRequest",
      defaultValue = "{\"sender\":\"string\",\"assetId\":\"Base58\",\"quantity\":100,\"fee\":100000}"
    )
  ))
  def burnRoute: Route =
    processRequest("burn", (b: BurnRequest) => transactionModule.burnAsset(b, wallet))


  private def balanceJson(address: String, assetIdStr: String): JsonResponse = {
    val account = new Account(address)
    Base58.decode(assetIdStr) match {
      case Success(assetId) if Account.isValid(account) =>
        val json = Json.obj(
          "address" -> account.address,
          "assetId" -> assetIdStr,
          "balance" -> state.assetBalance(AssetAcc(account, Some(assetId)))
        )
        JsonResponse(json, StatusCodes.OK)
      case _ => InvalidAddress.response
    }
  }

  private def balanceJson(address: String): JsonResponse = {
    val account = new Account(address)
    if (Account.isValid(account)) {
      val balances: Seq[JsObject] = state.getAccountBalance(account).map { p =>
        JsObject(Seq(
          "assetId" -> JsString(Base58.encode(p._1)),
          "balance" -> JsNumber(p._2._1),
          "reissuable" -> JsBoolean(p._2._2),
          "quantity" -> JsNumber(p._2._3),
          "issueTransaction" -> p._2._4.json
        ))
      }.toSeq
      val json = Json.obj(
        "address" -> account.address,
        "balances" -> JsArray(balances)
      )
      JsonResponse(json, StatusCodes.OK)
    } else InvalidAddress.response
  }

  @Path("/order")
  @ApiOperation(value = "Sign Order",
    notes = "Create order signed by address from wallet",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Order Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.assets.exchange.Order"
    )
  ))
  def signOrder: Route = processRequest("order", (order: Order) => {
    wallet.privateKeyAccount(order.senderPublicKey.address) match {
      case Some(sender) => Order.sign(order, sender)
      case None => InvalidAddress
    }
  })
}

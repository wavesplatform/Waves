package scorex.api.http.assets

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.RestAPISettings
import io.swagger.annotations._
import play.api.libs.json._
import scorex.account.Account
import scorex.api.http.{ApiError, ApiRoute, InvalidAddress}
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.Order
import scorex.transaction.assets.exchange.OrderJson._
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.{AssetAcc, AssetIdStringLength, TransactionOperations}
import scorex.wallet.Wallet

import scala.util.{Failure, Success}

@Path("/assets")
@Api(value = "assets")
case class AssetsApiRoute(settings: RestAPISettings, wallet: Wallet, state: StoredState, transactionModule: TransactionOperations) extends ApiRoute {
  val MaxAddressesPerRequest = 1000

  override lazy val route =
    pathPrefix("assets") {
      balance ~ balances ~ issue ~ reissue ~ burnRoute ~ transfer ~ signOrder ~ balanceDistribution
    }

  @Path("/balance/{address}/{assetId}")
  @ApiOperation(value = "Asset's balance", notes = "Account's balance by given asset", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "assetId", value = "Asset ID", required = true, dataType = "string", paramType = "path")
  ))
  def balance: Route =
    (get & path("balance" / Segment / Segment)) { (address, assetId) =>
      complete(balanceJson(address, assetId))
    }

  @Path("/{assetId}/distribution")
  @ApiOperation(value = "Asset balance distribution", notes = "Asset balance distribution by account", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "assetId", value = "Asset ID", required = true, dataType = "string", paramType = "path")
  ))
  def balanceDistribution: Route =
    (get & path(Segment / "distribution")) { assetId =>
      complete {
        Success(assetId).filter(_.length <= AssetIdStringLength).flatMap(Base58.decode) match {
          case Success(byteArray) => Json.toJson(state.assetDistribution(byteArray))
          case Failure(e) => ApiError.fromValidationError(scorex.transaction.ValidationError.CustomValidationError("Must be base58-encoded assetId"))
        }
      }
    }


  @Path("/balance/{address}")
  @ApiOperation(value = "Account's balance", notes = "Account's balances for all assets", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  def balances: Route =
    (get & path("balance" / Segment)) { address =>
      complete(balanceJson(address))
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
      dataType = "scorex.api.http.assets.TransferRequest",
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
      dataType = "scorex.api.http.assets.IssueRequest",
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
      dataType = "scorex.api.http.assets.ReissueRequest",
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
      dataType = "scorex.api.http.assets.BurnRequest",
      defaultValue = "{\"sender\":\"string\",\"assetId\":\"Base58\",\"quantity\":100,\"fee\":100000}"
    )
  ))
  def burnRoute: Route =
    processRequest("burn", (b: BurnRequest) => transactionModule.burnAsset(b, wallet))


  private def balanceJson(address: String, assetIdStr: String): Either[ApiError, JsObject] = {
    val account = new Account(address)
    Base58.decode(assetIdStr) match {
      case Success(assetId) if Account.isValid(account) =>
        val json = Json.obj(
          "address" -> account.address,
          "assetId" -> assetIdStr,
          "balance" -> state.assetBalance(AssetAcc(account, Some(assetId)))
        )
        Right(json)
      case _ => Left(InvalidAddress)
    }
  }

  private def balanceJson(address: String): Either[ApiError, JsObject] = {
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
      Right(json)
    } else Left(InvalidAddress)
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

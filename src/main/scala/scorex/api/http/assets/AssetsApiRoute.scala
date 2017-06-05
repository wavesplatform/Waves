package scorex.api.http.assets

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.StateReader
import io.swagger.annotations._
import play.api.libs.json._
import scorex.account.Account
import scorex.api.http.{ApiError, ApiRoute, InvalidAddress}
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.Order
import scorex.transaction.assets.exchange.OrderJson._
import scorex.transaction.{AssetAcc, AssetIdStringLength, NewTransactionHandler, TransactionFactory}
import scorex.utils.Time
import scorex.wallet.Wallet

import scala.util.{Failure, Success}

@Path("/assets")
@Api(value = "assets")
case class AssetsApiRoute(settings: RestAPISettings, wallet: Wallet, state: StateReader, newTxHandler: NewTransactionHandler, time: Time) extends ApiRoute {
  val MaxAddressesPerRequest = 1000

  override lazy val route =
    pathPrefix("assets") {
      balance ~ balances ~ issue ~ reissue ~ burnRoute ~ makeAssetNameUniqueRoute ~ transfer ~ signOrder ~ balanceDistribution ~ assetIdByUniqueName
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
          case Failure(e) => ApiError.fromValidationError(scorex.transaction.ValidationError.TransactionParameterValidationError("Must be base58-encoded assetId"))
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
      complete(fullAccountAssetsInfo(address))
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
    processRequest("transfer", (t: TransferRequest) => TransactionFactory.transferAsset(t, wallet, newTxHandler, time))

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
  def issue: Route = processRequest("issue", TransactionFactory.issueAsset(wallet, newTxHandler, time) _)

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
    processRequest("reissue", (r: ReissueRequest) => TransactionFactory.reissueAsset(r, wallet, newTxHandler, time))

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
    processRequest("burn", (b: BurnRequest) => TransactionFactory.burnAsset(b, wallet, newTxHandler, time))


  @Path("/make-asset-name-unique")
  @ApiOperation(value = "Make asset unique by name",
    notes = "Makes asset unique by name",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.assets.MakeAssetNameUniqueRequest",
      defaultValue = "{\"sender\":\"string\",\"assetId\":\"Base58\",\"fee\":100000}"
    )
  ))
  def makeAssetNameUniqueRoute: Route =
    processRequest("make-asset-name-unique", (b: MakeAssetNameUniqueRequest) => TransactionFactory.makeAssetNameUnique(b, wallet, newTxHandler, time))

  @Path("/asset-id-by-unique-name/{name}")
  @ApiOperation(value = "Asset id by unique name, if registered", notes = "Asset id by unique name, if registered", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "name", value = "Asset Name", required = true, dataType = "string", paramType = "path")
  ))
  def assetIdByUniqueName: Route =
    (get & path("asset-id-by-unique-name" / Segment)) { base58EncodedAssetName =>
      complete {
        Base58.decode(base58EncodedAssetName) match {
          case Success(assetName) => state.getAssetIdByUniqueName(ByteStr(assetName)) match {
            case Some(assetId) => JsString(assetId.base58)
            case None => JsNull
          }
          case Failure(e) => ApiError.fromValidationError(scorex.transaction.ValidationError.TransactionParameterValidationError("Must be base58-encoded assetId"))
        }
      }
    }


  private def balanceJson(address: String, assetIdStr: String): Either[ApiError, JsObject] = {
    ByteStr.decodeBase58(assetIdStr) match {
      case Success(assetId) =>
        (for {
          acc <- Account.fromString(address)
        } yield Json.obj(
          "address" -> acc.address,
          "assetId" -> assetIdStr,
          "balance" -> state.assetBalance(AssetAcc(acc, Some(assetId))))
          ).left.map(ApiError.fromValidationError)
      case _ => Left(InvalidAddress)
    }
  }

  private def fullAccountAssetsInfo(address: String): Either[ApiError, JsObject] = (for {
    acc <- Account.fromString(address)
  } yield {
    val balances: Seq[JsObject] = state.getAccountBalance(acc).map { case ((assetId, (balance, reissuable, quantity, issueTx, unique))) =>
      JsObject(Seq(
        "assetId" -> JsString(assetId.base58),
        "balance" -> JsNumber(balance),
        "reissuable" -> JsBoolean(reissuable),
        "quantity" -> JsNumber(quantity),
        "unique" -> JsBoolean(unique),
        "issueTransaction" -> issueTx.json
      ))
    }.toSeq
    Json.obj(
      "address" -> acc.address,
      "balances" -> JsArray(balances)
    )
  }).left.map(ApiError.fromValidationError)


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
    wallet.privateKeyAccount(order.senderPublicKey).map(pk => Order.sign(order, pk))
  })
}

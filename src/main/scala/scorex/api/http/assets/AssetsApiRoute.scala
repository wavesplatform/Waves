package scorex.api.http.assets

import akka.http.scaladsl.server.Route
import com.google.common.base.Charsets
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.diffs.CommonValidation
import com.wavesplatform.state.{Blockchain, ByteStr}
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json._
import scorex.BroadcastRoute
import scorex.account.Address
import scorex.api.http._
import com.wavesplatform.utils.Base58
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.assets.exchange.Order
import scorex.transaction.assets.exchange.OrderJson._
import scorex.transaction.smart.script.ScriptCompiler
import scorex.transaction.{AssetIdStringLength, TransactionFactory}
import scorex.utils.Time
import scorex.wallet.Wallet

import scala.concurrent.Future
import scala.util.{Failure, Success}

@Path("/assets")
@Api(value = "assets")
case class AssetsApiRoute(settings: RestAPISettings, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup, blockchain: Blockchain, time: Time)
    extends ApiRoute
    with BroadcastRoute {
  val MaxAddressesPerRequest = 1000

  override lazy val route =
    pathPrefix("assets") {
      balance ~ balances ~ issue ~ reissue ~ burnRoute ~ transfer ~ massTransfer ~ signOrder ~ balanceDistribution ~ details ~ sponsorRoute
    }

  @Path("/balance/{address}/{assetId}")
  @ApiOperation(value = "Asset's balance", notes = "Account's balance by given asset", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "assetId", value = "Asset ID", required = true, dataType = "string", paramType = "path")
    ))
  def balance: Route =
    (get & path("balance" / Segment / Segment)) { (address, assetId) =>
      complete(balanceJson(address, assetId))
    }

  @Path("/{assetId}/distribution")
  @ApiOperation(value = "Asset balance distribution", notes = "Asset balance distribution by account", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "assetId", value = "Asset ID", required = true, dataType = "string", paramType = "path")
    ))
  def balanceDistribution: Route =
    (get & path(Segment / "distribution")) { assetId =>
      complete {
        Success(assetId).filter(_.length <= AssetIdStringLength).flatMap(Base58.decode) match {
          case Success(byteArray) =>
            Json.toJson(blockchain.assetDistribution(ByteStr(byteArray)).map { case (a, b) => a.stringRepr -> b })
          case Failure(_) => ApiError.fromValidationError(scorex.transaction.ValidationError.GenericError("Must be base58-encoded assetId"))
        }
      }
    }

  @Path("/balance/{address}")
  @ApiOperation(value = "Account's balance", notes = "Account's balances for all assets", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    ))
  def balances: Route =
    (get & path("balance" / Segment)) { address =>
      complete(fullAccountAssetsInfo(address))
    }

  @Path("/details/{assetId}")
  @ApiOperation(value = "Information about an asset", notes = "Provides detailed information about given asset", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "assetId", value = "ID of the asset", required = true, dataType = "string", paramType = "path")
    ))
  def details: Route =
    (get & path("details" / Segment)) { id =>
      complete(assetDetails(id))
    }

  @Path("/transfer")
  @ApiOperation(value = "Transfer asset",
                notes = "Transfer asset to new address",
                httpMethod = "POST",
                produces = "application/json",
                consumes = "application/json")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "scorex.api.http.assets.TransferV2Request",
        defaultValue =
          "{\"sender\":\"3Mn6xomsZZepJj1GL1QaW6CaCJAq8B3oPef\",\"recipient\":\"3Mciuup51AxRrpSz7XhutnQYTkNT9691HAk\",\"assetId\":null,\"amount\":5813874260609385500,\"feeAssetId\":\"3Z7T9SwMbcBuZgcn3mGu7MMp619CTgSWBT7wvEkPwYXGnoYzLeTyh3EqZu1ibUhbUHAsGK5tdv9vJL9pk4fzv9Gc\",\"fee\":1579331567487095949,\"timestamp\":4231642878298810008}"
      )
    ))
  def transfer: Route =
    processRequest[TransferRequests](
      "transfer", { req =>
        req.eliminate(
          x => doBroadcast(TransactionFactory.transferAssetV1(x, wallet, time)),
          _.eliminate(
            x => doBroadcast(TransactionFactory.transferAssetV2(x, wallet, time)),
            _ => Future.successful(WrongJson(Some(new IllegalArgumentException("Doesn't know how to process request"))))
          )
        )
      }
    )

  @Path("/masstransfer")
  @ApiOperation(value = "Mass Transfer",
                notes = "Mass transfer of assets",
                httpMethod = "POST",
                produces = "application/json",
                consumes = "application/json")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "scorex.api.http.assets.MassTransferRequest",
        defaultValue =
          "{\"version\": 1, \"sender\":\"3Mn6xomsZZepJj1GL1QaW6CaCJAq8B3oPef\",\"transfers\":(\"3Mciuup51AxRrpSz7XhutnQYTkNT9691HAk\",100000000),\"fee\":100000,\"timestamp\":1517315595291}"
      )
    ))
  def massTransfer: Route =
    processRequest("masstransfer", (t: MassTransferRequest) => doBroadcast(TransactionFactory.massTransferAsset(t, wallet, time)))

  @Path("/issue")
  @ApiOperation(value = "Issue Asset", notes = "Issue new Asset", httpMethod = "POST", produces = "application/json", consumes = "application/json")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "scorex.api.http.assets.IssueV1Request",
        defaultValue =
          "{\"sender\":\"string\",\"name\":\"str\",\"description\":\"string\",\"quantity\":100000,\"decimals\":7,\"reissuable\":false,\"fee\":100000000}"
      )
    ))
  def issue: Route =
    processRequest("issue", (r: IssueV1Request) => doBroadcast(TransactionFactory.issueAssetV1(r, wallet, time)))

  @Path("/reissue")
  @ApiOperation(value = "Issue Asset", notes = "Reissue Asset", httpMethod = "POST", produces = "application/json", consumes = "application/json")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "scorex.api.http.assets.ReissueV1Request",
        defaultValue = "{\"sender\":\"string\",\"assetId\":\"Base58\",\"quantity\":100000,\"reissuable\":false,\"fee\":1}"
      )
    ))
  def reissue: Route =
    processRequest("reissue", (r: ReissueV1Request) => doBroadcast(TransactionFactory.reissueAssetV1(r, wallet, time)))

  @Path("/burn")
  @ApiOperation(value = "Burn Asset",
                notes = "Burn some of your assets",
                httpMethod = "POST",
                produces = "application/json",
                consumes = "application/json")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "scorex.api.http.assets.BurnV1Request",
        defaultValue = "{\"sender\":\"string\",\"assetId\":\"Base58\",\"quantity\":100,\"fee\":100000}"
      )
    ))
  def burnRoute: Route =
    processRequest("burn", (b: BurnV1Request) => doBroadcast(TransactionFactory.burnAssetV1(b, wallet, time)))

  @Path("/order")
  @ApiOperation(value = "Sign Order",
                notes = "Create order signed by address from wallet",
                httpMethod = "POST",
                produces = "application/json",
                consumes = "application/json")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Order Json with data",
        required = true,
        paramType = "body",
        dataType = "scorex.transaction.assets.exchange.Order"
      )
    ))
  def signOrder: Route =
    processRequest("order", (order: Order) => {
      wallet.privateKeyAccount(order.senderPublicKey).map(pk => Order.sign(order, pk))
    })

  private def balanceJson(address: String, assetIdStr: String): Either[ApiError, JsObject] = {
    ByteStr.decodeBase58(assetIdStr) match {
      case Success(assetId) =>
        (for {
          acc <- Address.fromString(address)
        } yield
          Json.obj("address" -> acc.address,
                   "assetId" -> assetIdStr,
                   "balance" -> JsNumber(BigDecimal(blockchain.portfolio(acc).assets.getOrElse(assetId, 0L))))).left.map(ApiError.fromValidationError)
      case _ => Left(InvalidAddress)
    }
  }

  private def fullAccountAssetsInfo(address: String): Either[ApiError, JsObject] =
    (for {
      acc <- Address.fromString(address)
    } yield {
      Json.obj(
        "address" -> acc.address,
        "balances" -> JsArray(
          (for {
            (assetId, balance) <- blockchain.portfolio(acc).assets
            if balance > 0
            assetInfo        <- blockchain.assetDescription(assetId)
            issueTransaction <- blockchain.transactionInfo(assetId)
          } yield
            Json.obj(
              "assetId"          -> assetId.base58,
              "balance"          -> balance,
              "reissuable"       -> assetInfo.reissuable,
              "quantity"         -> JsNumber(BigDecimal(assetInfo.totalVolume)),
              "issueTransaction" -> issueTransaction._2.json()
            )).toSeq)
      )
    }).left.map(ApiError.fromValidationError)

  private def assetDetails(assetId: String): Either[ApiError, JsObject] =
    (for {
      id <- ByteStr.decodeBase58(assetId).toOption.toRight("Incorrect asset ID")
      tt <- blockchain.transactionInfo(id).toRight("Failed to find issue transaction by ID")
      (h, mtx) = tt
      tx <- (mtx match {
        case t: IssueTransaction => Some(t)
        case _                   => None
      }).toRight("No issue transaction found with given asset ID")
      description <- blockchain.assetDescription(id).toRight("Failed to get description of the asset")
      complexity  <- description.script.fold[Either[String, Long]](Right(0))(ScriptCompiler.estimate)
    } yield {
      JsObject(
        Seq(
          "assetId"        -> JsString(id.base58),
          "issueHeight"    -> JsNumber(h),
          "issueTimestamp" -> JsNumber(tx.timestamp),
          "issuer"         -> JsString(tx.sender.toString),
          "name"           -> JsString(new String(tx.name, Charsets.UTF_8)),
          "description"    -> JsString(new String(tx.description, Charsets.UTF_8)),
          "decimals"       -> JsNumber(tx.decimals.toInt),
          "reissuable"     -> JsBoolean(description.reissuable),
          "quantity"       -> JsNumber(BigDecimal(description.totalVolume)),
          "script"         -> JsString(description.script.fold("")(_.bytes().base58)),
          "scriptText"     -> JsString(description.script.fold("")(_.text)),
          "complexity"     -> JsNumber(complexity),
          "extraFee"       -> JsNumber(if (description.script.isEmpty) 0 else CommonValidation.ScriptExtraFee),
          "minSponsoredAssetFee" -> (description.sponsorship match {
            case 0           => JsNull
            case sponsorship => JsNumber(sponsorship)
          })
        )
      )
    }).left.map(m => CustomValidationError(m))

  @Path("/sponsor")
  @ApiOperation(value = "Sponsor an Asset", httpMethod = "POST", produces = "application/json", consumes = "application/json")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "scorex.api.http.assets.SponsorFeeRequest",
        defaultValue = "{\"sender\":\"string\",\"assetId\":\"Base58\",\"minSponsoredAssetFee\":100000000,\"fee\":100000000}"
      )
    ))
  def sponsorRoute: Route =
    processRequest("sponsor", (req: SponsorFeeRequest) => doBroadcast(TransactionFactory.sponsor(req, wallet, time)))
}

package com.wavesplatform.api.http.assets

import java.util.concurrent._

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import com.google.common.base.Charsets
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.{CommonAccountsApi, CommonAssetsApi}
import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.api.http._
import com.wavesplatform.api.http.assets.AssetsApiRoute.DistributionParams
import com.wavesplatform.api.http.requests._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TransactionFactory
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.assets.exchange.OrderJson._
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.utils.{Time, _}
import com.wavesplatform.wallet.Wallet
import io.swagger.annotations._
import javax.ws.rs.Path
import monix.execution.Scheduler
import play.api.libs.json._

import scala.concurrent.Future

@Path("/assets")
@Api(value = "assets")
case class AssetsApiRoute(
    settings: RestAPISettings,
    wallet: Wallet,
    utxPoolSynchronizer: UtxPoolSynchronizer,
    blockchain: Blockchain,
    time: Time,
    commonAccountApi: CommonAccountsApi,
    commonAssetsApi: CommonAssetsApi
) extends ApiRoute
    with BroadcastRoute
    with AuthRoute {

  private[this] val distributionTaskScheduler = {
    val executor = new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue[Runnable](AssetsApiRoute.MAX_DISTRIBUTION_TASKS))
    Scheduler(executor)
  }

  override lazy val route: Route =
    pathPrefix("assets") {
      balance ~ balances ~ nft ~ balanceDistributionAtHeight ~ balanceDistribution ~ details ~ deprecatedRoute
    }

  @Path("/balance/{address}/{assetId}")
  @ApiOperation(value = "Asset's balance", notes = "Account's balance by given asset", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "assetId", value = "Asset ID", required = true, dataType = "string", paramType = "path")
    )
  )
  def balance: Route =
    (get & path("balance" / AddrSegment / TransactionId)) { (address, assetId) =>
      complete(balanceJson(address, assetId))
    }

  private def balanceDistribution(assetId: IssuedAsset, height: Int, limit: Int, after: Option[Address])(f: List[(Address, Long)] => JsValue) =
    complete {
      try {
        commonAccountApi
          .assetDistribution(assetId, height, after)
          .take(limit)
          .toListL
          .map(f)
          .runAsyncLogErr(distributionTaskScheduler)
      } catch {
        case _: RejectedExecutionException =>
          val errMsg = CustomValidationError("Asset distribution currently unavailable, try again later")
          Future.successful(errMsg.json: ToResponseMarshallable)
      }
    }

  def balanceDistribution: Route =
    (get & path(TransactionId / "distribution")) { assetId =>
      balanceDistribution(IssuedAsset(assetId), blockchain.height, settings.distributionAddressLimit, None) { l =>
        Json.toJson(l.map { case (a, b) => a.stringRepr -> b }.toMap)
      }
    }

  @Path("/{assetId}/distribution/{height}/limit/{limit}")
  @ApiOperation(
    value = "Asset balance distribution at height",
    notes = "Asset balance distribution by account at specified height",
    httpMethod = "GET"
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "assetId", value = "Asset ID", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "height", value = "Height", required = true, dataType = "integer", paramType = "path"),
      new ApiImplicitParam(name = "limit", value = "Number of addresses to be returned", required = true, dataType = "integer", paramType = "path"),
      new ApiImplicitParam(name = "after", value = "address to paginate after", required = false, dataType = "string", paramType = "query")
    )
  )
  def balanceDistributionAtHeight: Route =
    (get & path(TransactionId / "distribution" / IntNumber / "limit" / IntNumber) & parameter('after.?)) {
      (assetId, heightParam, limitParam, afterParam) =>
        val paramsEi: Either[ValidationError, DistributionParams] =
          AssetsApiRoute
            .validateDistributionParams(blockchain, heightParam, limitParam, settings.distributionAddressLimit, afterParam)

        paramsEi match {
          case Right((height, limit, after)) =>
            balanceDistribution(IssuedAsset(assetId), height, limit, after) { l =>
              Json.obj(
                "hasNext"  -> (l.length == limit),
                "lastItem" -> l.lastOption.map(_._1),
                "items"    -> Json.toJson(l.map { case (a, b) => a.stringRepr -> b }.toMap)
              )
            }
          case Left(error) => complete(error)
        }
    }

  @Path("/balance/{address}")
  @ApiOperation(value = "Account's balance", notes = "Account's balances for all assets", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    )
  )
  def balances: Route =
    (get & path("balance" / AddrSegment)) { address =>
      complete(fullAccountAssetsInfo(address))
    }

  @Path("/details/{assetId}")
  @ApiOperation(value = "Information about an asset", notes = "Provides detailed information about given asset", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "assetId", value = "ID of the asset", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "full", value = "false", required = false, dataType = "boolean", paramType = "query")
    )
  )
  def details: Route =
    (get & path("details" / TransactionId)) { id =>
      parameters('full.as[Boolean].?) { full =>
        complete(assetDetails(id, full.getOrElse(false)))
      }
    }

  @Path("/nft/{address}/limit/{limit}")
  @ApiOperation(value = "NFTs", notes = "Account's NFTs balance", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "limit", value = "Number of tokens to be returned", required = true, dataType = "integer", paramType = "path"),
      new ApiImplicitParam(name = "after", value = "Id of token to paginate after", required = false, dataType = "string", paramType = "query")
    )
  )
  def nft: Route =
    extractScheduler(
      implicit sc =>
        (path("nft" / AddrSegment / "limit" / IntNumber) & parameter('after.as[ByteStr].?) & get) { (address, limit, maybeAfter) =>
          complete {
            commonAccountApi
              .nftPortfolio(address, limit, maybeAfter.map(IssuedAsset))
              .map(_.json())
              .toListL
              .map(lst => JsArray(lst))
              .runAsyncLogErr
          }
        }
    )

  private def deprecatedRoute: Route =
    (path("transfer") & withAuth) {
      broadcast[TransferRequest](TransactionFactory.transferAsset(_, wallet, time))
    } ~ (path("masstransfer") & withAuth) {
      broadcast[MassTransferRequest](TransactionFactory.massTransferAsset(_, wallet, time))
    } ~ (path("issue") & withAuth) {
      broadcast[IssueRequest](TransactionFactory.issue(_, wallet, time))
    } ~ (path("reissue") & withAuth) {
      broadcast[ReissueRequest](TransactionFactory.reissue(_, wallet, time))
    } ~ (path("burn") & withAuth) {
      broadcast[BurnRequest](TransactionFactory.burn(_, wallet, time))
    } ~ (path("sponsor") & withAuth) {
      broadcast[SponsorFeeRequest](TransactionFactory.sponsor(_, wallet, time))
    } ~ (path("order") & withAuth)(jsonPost[Order] { order =>
      wallet.privateKeyAccount(order.senderPublicKey).map(pk => Order.sign(order, pk))
    }) ~ pathPrefix("broadcast")(
      path("issue")(broadcast[IssueRequest](_.toTx)) ~
        path("reissue")(broadcast[ReissueRequest](_.toTx)) ~
        path("burn")(broadcast[BurnRequest](_.toTx)) ~
        path("exchange")(broadcast[ExchangeRequest](_.toTx)) ~
        path("transfer")(broadcast[TransferRequest](_.toTx))
    )

  private def balanceJson(address: Address, assetId: ByteStr): JsObject =
    Json.obj(
      "address" -> address,
      "assetId" -> assetId,
      "balance" -> JsNumber(BigDecimal(blockchain.balance(address, IssuedAsset(assetId))))
    )

  private def fullAccountAssetsInfo(acc: Address): JsObject =
    Json.obj(
      "address" -> acc.stringRepr,
      "balances" -> JsArray(
        (for {
          (asset @ IssuedAsset(assetId), balance)                                <- commonAccountApi.portfolio(acc)
          CommonAssetsApi.AssetInfo(assetInfo, issueTransaction, sponsorBalance) <- commonAssetsApi.fullInfo(asset)
        } yield Json.obj(
          "assetId"    -> assetId,
          "balance"    -> balance,
          "reissuable" -> assetInfo.reissuable,
          "minSponsoredAssetFee" -> (assetInfo.sponsorship match {
            case 0           => JsNull
            case sponsorship => JsNumber(sponsorship)
          }),
          "sponsorBalance"   -> sponsorBalance,
          "quantity"         -> JsNumber(BigDecimal(assetInfo.totalVolume)),
          "issueTransaction" -> issueTransaction.json()
        )).toSeq
      )
    )

  private def assetDetails(id: ByteStr, full: Boolean): Either[ApiError, JsObject] =
    (for {
      tt <- blockchain.transactionInfo(id).toRight("Failed to find issue transaction by ID")
      (h, mtx) = tt
      tx <- (mtx match {
        case t: IssueTransaction => Some(t)
        case _                   => None
      }).toRight("No issue transaction found with given asset ID")
      description <- blockchain.assetDescription(IssuedAsset(id)).toRight("Failed to get description of the asset")
      script = description.script.filter(_ => full)
      complexity <- script.fold[Either[String, Long]](Right(0))(script => ScriptCompiler.estimate(script, script.stdLibVersion))
    } yield {
      JsObject(
        Seq(
          "assetId"        -> JsString(id.toString),
          "issueHeight"    -> JsNumber(h),
          "issueTimestamp" -> JsNumber(tx.timestamp),
          "issuer"         -> JsString(tx.sender.stringRepr),
          "name"           -> JsString(new String(tx.name, Charsets.UTF_8)),
          "description"    -> JsString(new String(tx.description, Charsets.UTF_8)),
          "decimals"       -> JsNumber(tx.decimals.toInt),
          "reissuable"     -> JsBoolean(description.reissuable),
          "quantity"       -> JsNumber(BigDecimal(description.totalVolume)),
          "scripted"       -> JsBoolean(description.script.nonEmpty),
          "minSponsoredAssetFee" -> (description.sponsorship match {
            case 0           => JsNull
            case sponsorship => JsNumber(sponsorship)
          })
        ) ++ script.toSeq.map { script =>
          "scriptDetails" -> Json.obj(
            "scriptComplexity" -> JsNumber(BigDecimal(complexity)),
            "script"           -> JsString(script.bytes().base64),
            "scriptText"       -> JsString(script.expr.toString) // [WAIT] JsString(Script.decompile(script))
          )
        }
      )
    }).left.map(m => CustomValidationError(m))
}

object AssetsApiRoute {
  val MAX_DISTRIBUTION_TASKS = 5

  type DistributionParams = (Int, Int, Option[Address])

  def validateDistributionParams(
      blockchain: Blockchain,
      heightParam: Int,
      limitParam: Int,
      maxLimit: Int,
      afterParam: Option[String]
  ): Either[ValidationError, DistributionParams] = {
    for {
      limit  <- validateLimit(limitParam, maxLimit)
      height <- validateHeight(blockchain, heightParam)
      after <- afterParam
        .fold[Either[ValidationError, Option[Address]]](Right(None))(addrString => Address.fromString(addrString).map(Some(_)))
    } yield (height, limit, after)
  }

  def validateHeight(blockchain: Blockchain, height: Int): Either[ValidationError, Int] = {
    for {
      _ <- Either
        .cond(height > 0, (), GenericError(s"Height should be greater than zero"))
      _ <- Either
        .cond(height != blockchain.height, (), GenericError(s"Using 'assetDistributionAtHeight' on current height can lead to inconsistent result"))
      _ <- Either
        .cond(height < blockchain.height, (), GenericError(s"Asset distribution available only at height not greater than ${blockchain.height - 1}"))
    } yield height

  }

  def validateLimit(limit: Int, maxLimit: Int): Either[ValidationError, Int] = {
    for {
      _ <- Either
        .cond(limit > 0, (), GenericError("Limit should be greater than 0"))
      _ <- Either
        .cond(limit < maxLimit, (), GenericError(s"Limit should be less than $maxLimit"))
    } yield limit
  }
}

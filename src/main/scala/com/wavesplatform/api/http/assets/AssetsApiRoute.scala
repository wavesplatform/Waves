package com.wavesplatform.api.http.assets

import java.util.concurrent._

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import cats.instances.either.catsStdInstancesForEither
import cats.instances.option.catsStdInstancesForOption
import cats.syntax.either._
import cats.syntax.traverse._
import com.google.common.base.Charsets
import com.wavesplatform.account.Address
import com.wavesplatform.api.http._
import com.wavesplatform.api.http.assets.AssetsApiRoute.DistributionParams
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.assets.exchange.OrderJson._
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{AssetIdStringLength, TransactionFactory, ValidationError}
import com.wavesplatform.utils.{Time, _}
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import javax.ws.rs.Path
import monix.eval.Task
import monix.execution.Scheduler
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Success

@Path("/assets")
@Api(value = "assets")
case class AssetsApiRoute(settings: RestAPISettings, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup, blockchain: Blockchain, time: Time)
    extends ApiRoute
    with BroadcastRoute {

  private val distributionTaskScheduler = {
    val executor = new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue[Runnable](AssetsApiRoute.MAX_DISTRIBUTION_TASKS))
    Scheduler(executor)
  }

  override lazy val route =
    pathPrefix("assets") {
      balance ~ balances ~ issue ~ reissue ~ burnRoute ~ transfer ~ massTransfer ~ signOrder ~ balanceDistributionAtHeight ~ balanceDistribution ~ details ~ sponsorRoute
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

  def assetDistributionTask(params: DistributionParams): Task[ToResponseMarshallable] = {
    val (asset, height, limit, maybeAfter) = params

    val distributionTask = Task.eval(
      blockchain
        .assetDistributionAtHeight(asset, height, limit, maybeAfter)
    )

    distributionTask.map {
      case Right(dst) => Json.toJson(dst): ToResponseMarshallable
      case Left(err)  => ApiError.fromValidationError(err)
    }
  }

  @Deprecated
  @Path("/{assetId}/distribution")
  @ApiOperation(value = "Asset balance distribution", notes = "Asset balance distribution by account", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "assetId", value = "Asset ID", required = true, dataType = "string", paramType = "path")
    ))
  def balanceDistribution: Route =
    (get & path(Segment / "distribution")) { (assetParam) =>
      val assetEi = AssetsApiRoute
        .validateAssetId(assetParam)

      val distributionTask = assetEi match {
        case Left(err) => Task.pure(ApiError.fromValidationError(err): ToResponseMarshallable)
        case Right(asset) =>
          Task
            .eval(blockchain.assetDistribution(asset))
            .map(dst => Json.toJson(dst): ToResponseMarshallable)
      }

      complete {
        try {
          distributionTask.runAsyncLogErr(distributionTaskScheduler)
        } catch {
          case _: RejectedExecutionException =>
            val errMsg = CustomValidationError("Asset distribution currently unavailable, try again later")
            Future.successful(errMsg.json: ToResponseMarshallable)
        }
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
    ))
  def balanceDistributionAtHeight: Route =
    (get & path(Segment / "distribution" / IntNumber / "limit" / IntNumber) & parameter('after.?)) {
      (assetParam, heightParam, limitParam, afterParam) =>
        val paramsEi: Either[ValidationError, DistributionParams] =
          AssetsApiRoute
            .validateDistributionParams(blockchain, assetParam, heightParam, limitParam, settings.distributionAddressLimit, afterParam)

        val resultTask = paramsEi match {
          case Left(err)     => Task.pure(ApiError.fromValidationError(err): ToResponseMarshallable)
          case Right(params) => assetDistributionTask(params)
        }

        complete {
          try {
            resultTask.runAsyncLogErr(distributionTaskScheduler)
          } catch {
            case _: RejectedExecutionException =>
              val errMsg = CustomValidationError("Asset distribution currently unavailable, try again later")
              Future.successful(errMsg.json: ToResponseMarshallable)
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
      new ApiImplicitParam(name = "assetId", value = "ID of the asset", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "full", value = "false", required = false, dataType = "boolean", paramType = "query")
    ))
  def details: Route =
    (get & path("details" / Segment)) { id =>
      parameters('full.as[Boolean].?) { full =>
        complete(assetDetails(id, full.getOrElse(false)))
      }
    }

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

  def massTransfer: Route =
    processRequest("masstransfer", (t: MassTransferRequest) => doBroadcast(TransactionFactory.massTransferAsset(t, wallet, time)))

  def issue: Route =
    processRequest("issue", (r: IssueV1Request) => doBroadcast(TransactionFactory.issueAssetV1(r, wallet, time)))

  def reissue: Route =
    processRequest("reissue", (r: ReissueV1Request) => doBroadcast(TransactionFactory.reissueAssetV1(r, wallet, time)))

  def burnRoute: Route =
    processRequest("burn", (b: BurnV1Request) => doBroadcast(TransactionFactory.burnAssetV1(b, wallet, time)))

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
                   "balance" -> JsNumber(BigDecimal(blockchain.balance(acc, IssuedAsset(assetId)))))).left
          .map(ApiError.fromValidationError)
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
            (asset @ IssuedAsset(assetId), balance) <- blockchain.portfolio(acc).assets
            if balance > 0
            assetInfo                               <- blockchain.assetDescription(asset)
            (_, issueTransaction: IssueTransaction) <- blockchain.transactionInfo(assetId)
            sponsorBalance = if (assetInfo.sponsorship != 0) {
              Some(blockchain.wavesPortfolio(issueTransaction.sender).spendableBalance)
            } else {
              None
            }
          } yield
            Json.obj(
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
            )).toSeq)
      )
    }).left.map(ApiError.fromValidationError)

  private def assetDetails(assetId: String, full: Boolean): Either[ApiError, JsObject] =
    (for {
      id <- ByteStr.decodeBase58(assetId).toOption.toRight("Incorrect asset ID")
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
          "assetId"        -> JsString(id.base58),
          "issueHeight"    -> JsNumber(h),
          "issueTimestamp" -> JsNumber(tx.timestamp),
          "issuer"         -> JsString(tx.sender.toString),
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

  def sponsorRoute: Route =
    processRequest("sponsor", (req: SponsorFeeRequest) => doBroadcast(TransactionFactory.sponsor(req, wallet, time)))
}

object AssetsApiRoute {
  val MAX_DISTRIBUTION_TASKS = 5

  type DistributionParams = (IssuedAsset, Int, Int, Option[Address])

  def validateDistributionParams(blockchain: Blockchain,
                                 assetParam: String,
                                 heightParam: Int,
                                 limitParam: Int,
                                 maxLimit: Int,
                                 afterParam: Option[String]): Either[ValidationError, DistributionParams] = {
    for {
      limit   <- validateLimit(limitParam, maxLimit)
      height  <- validateHeight(blockchain, heightParam)
      assetId <- validateAssetId(assetParam)
      after   <- afterParam.traverse[Either[ValidationError, ?], Address](Address.fromString)
    } yield (assetId, height, limit, after)
  }

  def validateAssetId(assetParam: String): Either[ValidationError, IssuedAsset] = {
    for {
      _ <- Either.cond(assetParam.length <= AssetIdStringLength, (), GenericError("Unexpected assetId length"))
      assetId <- Base58
        .tryDecodeWithLimit(assetParam)
        .fold(
          _ => GenericError("Must be base58-encoded assetId").asLeft[IssuedAsset],
          arr => IssuedAsset(ByteStr(arr)).asRight[ValidationError]
        )
    } yield assetId
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

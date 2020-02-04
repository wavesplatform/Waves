package com.wavesplatform.api.http.assets

import java.util.concurrent._

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import cats.instances.either.catsStdInstancesForEither
import cats.instances.option.catsStdInstancesForOption
import cats.syntax.either._
import cats.syntax.traverse._
import com.google.common.base.Charsets
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.{CommonAccountApi, CommonAssetsApi}
import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.api.http._
import com.wavesplatform.api.http.assets.AssetsApiRoute.DistributionParams
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.http.{BroadcastRoute, CustomJson}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.assets.exchange.OrderJson._
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{AssetIdStringLength, TransactionFactory, TxValidationError}
import com.wavesplatform.utils.{Time, _}
import com.wavesplatform.wallet.Wallet
import monix.eval.Task
import monix.execution.Scheduler
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Success

case class AssetsApiRoute(settings: RestAPISettings, wallet: Wallet, utxPoolSynchronizer: UtxPoolSynchronizer, blockchain: Blockchain, time: Time)
    extends ApiRoute
    with BroadcastRoute
    with AuthRoute {

  private[this] val commonAccountApi = new CommonAccountApi(blockchain)
  private[this] val commonAssetsApi  = new CommonAssetsApi(blockchain)

  private[this] val distributionTaskScheduler = {
    val executor = new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue[Runnable](AssetsApiRoute.MAX_DISTRIBUTION_TASKS))
    Scheduler(executor)
  }

  override lazy val route: Route =
    pathPrefix("assets") {
      balance ~ balances ~ nft ~ balanceDistributionAtHeight ~ balanceDistribution ~ details ~ deprecatedRoute
    }

  def balance: Route =
    (get & path("balance" / Segment / Segment)) { (address, assetId) =>
      complete(balanceJson(address, assetId))
    }

  def assetDistributionTask(params: DistributionParams)(renderNumbersAsStrings: Boolean): Task[ToResponseMarshallable] = {
    val (asset, height, limit, maybeAfter) = params

    val distributionTask = Task.eval(
      blockchain.assetDistributionAtHeight(asset, height, limit, maybeAfter).map { adp =>
        if (renderNumbersAsStrings)
          Json.obj(
            "hasNext" -> adp.hasNext,
            "last"    -> adp.lastItem.map(_.stringRepr),
            "items"   -> Json.toJson(adp.items.mapValues(_.toString))
          )
        else Json.toJson(adp)
      }
    )

    distributionTask.map {
      case Right(dst) => dst: ToResponseMarshallable
      case Left(err)  => ApiError.fromValidationError(err)
    }
  }

  def balanceDistribution: Route =
    (get & path(Segment / "distribution")) { (assetParam) =>
      val assetEi = AssetsApiRoute
        .validateAssetId(assetParam)

      val distributionTask = assetEi match {
        case Left(err) => Task.pure(ApiError.fromValidationError(err): ToResponseMarshallable)
        case Right(asset) =>
          Task
            .eval(blockchain.assetDistribution(asset))
            .map(dst => Json.toJson(dst)(com.wavesplatform.state.dstWrites): ToResponseMarshallable)
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

  def balanceDistributionAtHeight: Route =
    (get & path(Segment / "distribution" / IntNumber / "limit" / IntNumber) & parameter('after.?)) {
      (assetParam, heightParam, limitParam, afterParam) =>
        optionalHeaderValueByType[Accept](()) { maybeAccept =>
          val paramsEi: Either[ValidationError, DistributionParams] =
            AssetsApiRoute
              .validateDistributionParams(blockchain, assetParam, heightParam, limitParam, settings.distributionAddressLimit, afterParam)

          val resultTask = paramsEi match {
            case Left(err) => Task.pure(ApiError.fromValidationError(err): ToResponseMarshallable)
            case Right(params) =>
              assetDistributionTask(params)(maybeAccept.exists(_.mediaRanges.exists(CustomJson.acceptsNumbersAsStrings)))
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
    }

  def balances: Route =
    (get & path("balance" / Segment)) { address =>
      complete(fullAccountAssetsInfo(address))
    }

  def details: Route =
    (get & path("details" / Segment)) { id =>
      parameters('full.as[Boolean].?) { full =>
        complete(assetDetails(id, full.getOrElse(false)))
      }
    }

  def nft: Route =
    extractScheduler(
      implicit sc =>
        (path("nft" / Segment / "limit" / IntNumber) & parameter('after.?) & get) { (addressParam, limitParam, maybeAfterParam) =>
          val response: Either[ApiError, Future[JsArray]] = for {
            addr  <- Address.fromString(addressParam).left.map(ApiError.fromValidationError)
            limit <- Either.cond(limitParam <= settings.transactionsByAddressLimit, limitParam, TooBigArrayAllocation)
            maybeAfter <- maybeAfterParam match {
              case Some(v) =>
                ByteStr
                  .decodeBase58(v)
                  .fold(
                    _ => Left(CustomValidationError(s"Unable to decode asset id $v")),
                    id => Right(Some(IssuedAsset(id)))
                  )
              case None => Right(None)
            }
          } yield {
            commonAccountApi
              .portfolioNFT(addr, maybeAfter)
              .take(limit)
              .map(_.json())
              .toListL
              .map(lst => JsArray(lst))
              .runAsyncLogErr
          }

          complete(response)
        }
    )

  private def deprecatedRoute: Route =
    (path("transfer") & withAuth) {
      broadcast[TransferRequests](
        _.eliminate(
          v1 => TransactionFactory.transferAssetV1(v1, wallet, time),
          _.eliminate(v2 => TransactionFactory.transferAssetV2(v2, wallet, time), _ => Left(TxValidationError.UnsupportedTransactionType))
        )
      )
    } ~ (path("masstransfer") & withAuth) {
      broadcast[MassTransferRequest](TransactionFactory.massTransferAsset(_, wallet, time))
    } ~ (path("issue") & withAuth) {
      broadcast[IssueV1Request](TransactionFactory.issueAssetV1(_, wallet, time))
    } ~ (path("reissue") & withAuth) {
      broadcast[ReissueV1Request](TransactionFactory.reissueAssetV1(_, wallet, time))
    } ~ (path("burn") & withAuth) {
      broadcast[BurnV1Request](TransactionFactory.burnAssetV1(_, wallet, time))
    } ~ (path("sponsor") & withAuth) {
      broadcast[SponsorFeeRequest](TransactionFactory.sponsor(_, wallet, time))
    } ~ (path("order") & withAuth)(jsonPost[Order] { order =>
      wallet.privateKeyAccount(order.senderPublicKey).map(pk => Order.sign(order, pk))
    }) ~ pathPrefix("broadcast")(
      path("issue")(broadcast[SignedIssueV1Request](_.toTx)) ~
        path("reissue")(broadcast[SignedReissueV1Request](_.toTx)) ~
        path("burn")(broadcast[SignedBurnV1Request](_.toTx)) ~
        path("exchange")(broadcast[SignedExchangeRequest](_.toTx)) ~
        path("transfer")(
          broadcast[SignedTransferRequests](
            _.eliminate(
              _.toTx,
              _.eliminate(
                _.toTx,
                _ => Left(TxValidationError.UnsupportedTransactionType)
              )
            )
          )
        )
    )

  private def balanceJson(address: String, assetIdStr: String): Either[ApiError, JsObject] = {
    ByteStr.decodeBase58(assetIdStr) match {
      case Success(assetId) =>
        (for {
          acc <- Address.fromString(address)
        } yield Json.obj(
          "address" -> acc.stringRepr,
          "assetId" -> assetIdStr,
          "balance" -> JsNumber(BigDecimal(blockchain.balance(acc, IssuedAsset(assetId))))
        )).left
          .map(ApiError.fromValidationError)
      case _ => Left(InvalidAddress)
    }
  }

  private def fullAccountAssetsInfo(address: String): Either[ApiError, JsObject] =
    (for {
      acc <- Address.fromString(address)
    } yield {
      Json.obj(
        "address" -> acc.stringRepr,
        "balances" -> JsArray(
          (for {
            (asset @ IssuedAsset(assetId), balance)                                <- commonAccountApi.portfolio(acc) if balance > 0
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

  type DistributionParams = (IssuedAsset, Int, Int, Option[Address])

  def validateDistributionParams(
      blockchain: Blockchain,
      assetParam: String,
      heightParam: Int,
      limitParam: Int,
      maxLimit: Int,
      afterParam: Option[String]
  ): Either[ValidationError, DistributionParams] = {
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

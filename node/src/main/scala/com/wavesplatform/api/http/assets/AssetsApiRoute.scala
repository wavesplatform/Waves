package com.wavesplatform.api.http.assets

import java.util.concurrent._

import scala.concurrent.Future

import akka.NotUsed
import akka.http.scaladsl.marshalling.{ToResponseMarshallable, ToResponseMarshaller}
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.Source
import cats.instances.either._
import cats.instances.list._
import cats.syntax.alternative._
import cats.syntax.either._
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.{CommonAccountsApi, CommonAssetsApi}
import com.wavesplatform.api.http._
import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.api.http.assets.AssetsApiRoute.DistributionParams
import com.wavesplatform.api.http.requests._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.{AssetDescription, AssetScriptInfo, Blockchain}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TransactionFactory
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.assets.exchange.OrderJson._
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import io.netty.util.concurrent.DefaultThreadFactory
import monix.execution.Scheduler
import monix.reactive.Observable
import play.api.libs.json._

case class AssetsApiRoute(
    settings: RestAPISettings,
    wallet: Wallet,
    transactionPublisher: TransactionPublisher,
    blockchain: Blockchain,
    time: Time,
    commonAccountApi: CommonAccountsApi,
    commonAssetsApi: CommonAssetsApi,
    maxDistributionDepth: Int
) extends ApiRoute
    with BroadcastRoute
    with AuthRoute {

  private[this] val distributionTaskScheduler = Scheduler(
    new ThreadPoolExecutor(
      1,
      1,
      0L,
      TimeUnit.MILLISECONDS,
      new LinkedBlockingQueue[Runnable](AssetsApiRoute.MAX_DISTRIBUTION_TASKS),
      new DefaultThreadFactory("balance-distribution", true)
    )
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
      wallet.privateKeyAccount(order.senderPublicKey.toAddress).map(pk => Order.sign(order, pk.privateKey))
    }) ~ pathPrefix("broadcast")(
      path("issue")(broadcast[IssueRequest](_.toTx)) ~
        path("reissue")(broadcast[ReissueRequest](_.toTx)) ~
        path("burn")(broadcast[BurnRequest](_.toTx)) ~
        path("exchange")(broadcast[ExchangeRequest](_.toTx)) ~
        path("transfer")(broadcast[TransferRequest](_.toTx))
    )

  override lazy val route: Route =
    pathPrefix("assets") {
      get {
        pathPrefix("balance") {
          pathPrefix(AddrSegment) { address =>
            pathEndOrSingleSlash {
              balances(address)
            } ~
              path(AssetId) { assetId =>
                balance(address, assetId)
              }
          }
        } ~ pathPrefix("details") {
          (pathEndOrSingleSlash & parameters(("id".as[String].*, "full".as[Boolean] ? false))) { (ids, full) =>
            multipleDetailsGet(ids.toSeq.reverse, full)
          } ~ (path(AssetId) & parameter("full".as[Boolean] ? false)) { (assetId, full) =>
            singleDetails(assetId, full)
          }
        } ~ (path("nft" / AddrSegment / "limit" / IntNumber) & parameter("after".as[String].?)) { (address, limit, maybeAfter) =>
          nft(address, limit, maybeAfter)
        } ~ pathPrefix(AssetId / "distribution") { assetId =>
          pathEndOrSingleSlash(balanceDistribution(assetId)) ~
            (path(IntNumber / "limit" / IntNumber) & parameter("after".?)) { (height, limit, maybeAfter) =>
              balanceDistributionAtHeight(assetId, height, limit, maybeAfter)
            }
        }
      } ~ post {
        (path("details") & parameter("full".as[Boolean] ? false)) { full =>
          jsonPost[JsObject] { jsv =>
            (jsv \ "ids").validate[List[String]] match {
              case JsSuccess(ids, _) =>
                ids.map(id => ByteStr.decodeBase58(id).toEither.leftMap(_ => id)).separate match {
                  case (Nil, Nil)      => CustomValidationError("Empty request")
                  case (Nil, assetIds) => assetIds.map(id => assetDetails(IssuedAsset(id), full).fold(_.json, identity))
                  case (errors, _)     => InvalidIds(errors)
                }
              case JsError(err) => WrongJson(errors = err)
            }
          }
        } ~ deprecatedRoute
      }
    }

  def balances(address: Address): Route = extractScheduler { implicit s =>
    implicit val jsonStreamingSupport: ToResponseMarshaller[Source[JsObject, NotUsed]] =
      jsonStreamMarshaller(s"""{"address":"$address","balances":[""", ",", "]}")

    complete(commonAccountApi.portfolio(address).toListL.runToFuture.map { balances =>
      Source.fromIterator(
        () =>
          balances.iterator.flatMap {
            case (assetId, balance) =>
              commonAssetsApi.fullInfo(assetId).map {
                case CommonAssetsApi.AssetInfo(assetInfo, issueTransaction, sponsorBalance) =>
                  Json.obj(
                    "assetId"    -> assetId.id.toString,
                    "balance"    -> balance,
                    "reissuable" -> assetInfo.reissuable,
                    "minSponsoredAssetFee" -> (assetInfo.sponsorship match {
                      case 0           => JsNull
                      case sponsorship => JsNumber(sponsorship)
                    }),
                    "sponsorBalance"   -> sponsorBalance,
                    "quantity"         -> JsNumber(BigDecimal(assetInfo.totalVolume)),
                    "issueTransaction" -> issueTransaction.map(_.json())
                  )
              }
          }
      )
    })
  }

  def balance(address: Address, assetId: IssuedAsset): Route = complete(balanceJson(address, assetId))

  private def balanceDistribution(assetId: IssuedAsset, height: Int, limit: Int, after: Option[Address])(f: List[(Address, Long)] => JsValue) =
    complete {
      try {
        commonAssetsApi
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

  def balanceDistribution(assetId: IssuedAsset): Route =
    balanceDistribution(assetId, blockchain.height, Int.MaxValue, None) { l =>
      Json.toJson(l.map { case (a, b) => a.stringRepr -> b }.toMap)
    }

  def balanceDistributionAtHeight(assetId: IssuedAsset, heightParam: Int, limitParam: Int, afterParam: Option[String]): Route =
    optionalHeaderValueByType[Accept](()) { accept =>
      val paramsEi: Either[ValidationError, DistributionParams] =
        AssetsApiRoute
          .validateDistributionParams(blockchain, heightParam, limitParam, settings.distributionAddressLimit, afterParam, maxDistributionDepth)

      paramsEi match {
        case Right((height, limit, after)) =>
          balanceDistribution(assetId, height, limit, after) { l =>
            Json.obj(
              "hasNext"  -> (l.length == limit),
              "lastItem" -> l.lastOption.map(_._1),
              "items" -> Json.toJson(l.map {
                case (a, b) =>
                  a.stringRepr -> accept.fold[JsValue](JsNumber(b)) {
                    case a if a.mediaRanges.exists(CustomJson.acceptsNumbersAsStrings) => JsString(b.toString)
                    case _                                                             => JsNumber(b)
                  }
              }.toMap)
            )
          }
        case Left(error) => complete(error)
      }
    }

  def singleDetails(assetId: IssuedAsset, full: Boolean): Route = complete(assetDetails(assetId, full))

  def multipleDetailsGet(ids: Seq[String], full: Boolean): Route =
    complete(ids.toList.map(id => assetDetails(IssuedAsset(ByteStr.decodeBase58(id).get), full).fold(_.json, identity)))

  def multipleDetailsPost(full: Boolean): Route =
    entity(as[JsObject]) { jsv =>
      complete(
        (jsv \ "ids").validate[List[ByteStr]] match {
          case JsSuccess(ids, _) => Json.arr(ids.map(id => assetDetails(IssuedAsset(id), full).fold(_.json, identity)))
          case JsError(err)      => WrongJson(errors = err)
        }
      )
    }

  def nft(address: Address, limit: Int, maybeAfter: Option[String]): Route = {
    val after = maybeAfter.collect { case s if s.nonEmpty => IssuedAsset(ByteStr.decodeBase58(s).getOrElse(throw ApiException(InvalidAssetId))) }
    if (limit > settings.transactionsByAddressLimit) complete(TooBigArrayAllocation)
    else
      extractScheduler { implicit sc =>
        implicit val jsonStreamingSupport: ToResponseMarshaller[Source[JsValue, NotUsed]] = jsonStreamMarshaller()
        complete {
          Source.fromPublisher(
            commonAccountApi
              .nftList(address, after)
              .flatMap {
                case (assetId, assetDesc) =>
                  Observable.fromEither(
                    AssetsApiRoute
                      .jsonDetails(blockchain)(assetId, assetDesc, true)
                      .leftMap(err => new IllegalArgumentException(err))
                  )
              }
              .take(limit)
              .toReactivePublisher
          )
        }
      }
  }

  private def balanceJson(address: Address, assetId: IssuedAsset): JsObject =
    Json.obj(
      "address" -> address,
      "assetId" -> assetId.id.toString,
      "balance" -> JsNumber(BigDecimal(blockchain.balance(address, assetId)))
    )

  private def assetDetails(assetId: IssuedAsset, full: Boolean): Either[ApiError, JsObject] = {
    for {
      description <- blockchain.assetDescription(assetId).toRight(AssetDoesNotExist(assetId))
      result      <- AssetsApiRoute.jsonDetails(blockchain)(assetId, description, full).leftMap(CustomValidationError(_))
    } yield result
  }
}

object AssetsApiRoute {
  val MAX_DISTRIBUTION_TASKS = 5

  type DistributionParams = (Int, Int, Option[Address])

  def validateDistributionParams(
      blockchain: Blockchain,
      heightParam: Int,
      limitParam: Int,
      maxLimit: Int,
      afterParam: Option[String],
      maxDistributionDepth: Int
  ): Either[ValidationError, DistributionParams] = {
    for {
      limit  <- validateLimit(limitParam, maxLimit)
      height <- validateHeight(blockchain, heightParam, maxDistributionDepth)
      after <- afterParam
        .fold[Either[ValidationError, Option[Address]]](Right(None))(addrString => Address.fromString(addrString).map(Some(_)))
    } yield (height, limit, after)
  }

  def validateHeight(blockchain: Blockchain, height: Int, maxDistributionDepth: Int): Either[ValidationError, Int] = {
    for {
      _ <- Either.cond(height > 0, (), GenericError(s"Height should be greater than zero"))
      _ <- Either.cond(
        height != blockchain.height,
        (),
        GenericError(s"Using 'assetDistributionAtHeight' on current height can lead to inconsistent result")
      )
      _ <- Either.cond(
        height < blockchain.height,
        (),
        GenericError(s"Asset distribution available only at height not greater than ${blockchain.height - 1}")
      )
      _ <- Either
        .cond(
          height >= blockchain.height - maxDistributionDepth,
          (),
          GenericError(s"Unable to get distribution past height ${blockchain.height - maxDistributionDepth}")
        )
    } yield height

  }

  def validateLimit(limit: Int, maxLimit: Int): Either[ValidationError, Int] = {
    for {
      _ <- Either.cond(limit > 0, (), GenericError("Limit should be greater than 0"))
      _ <- Either.cond(limit <= maxLimit, (), GenericError(s"Limit should be less than or equal to $maxLimit"))
    } yield limit
  }

  def jsonDetails(blockchain: Blockchain)(id: IssuedAsset, description: AssetDescription, full: Boolean): Either[String, JsObject] = {
    // (timestamp, height)
    def additionalInfo(id: ByteStr): Either[String, (Long, Int)] =
      for {
        tt <- blockchain
          .transactionInfo(id)
          .filter { case (_, _, confirmed) => confirmed }
          .toRight("Failed to find issue/invokeScript transaction by ID")
        (h, mtx, _) = tt
        ts <- (mtx match {
          case tx: IssueTransaction        => Some(tx.timestamp)
          case tx: InvokeScriptTransaction => Some(tx.timestamp)
          case _                           => None
        }).toRight("No issue/invokeScript transaction found with the given asset ID")
      } yield (ts, h)

    for {
      tsh <- additionalInfo(description.originTransactionId)
      (timestamp, height) = tsh
      script              = description.script.filter(_ => full)
      name                = description.name.toStringUtf8
      desc                = description.description.toStringUtf8
    } yield JsObject(
      Seq(
        "assetId"         -> JsString(id.id.toString),
        "issueHeight"     -> JsNumber(height),
        "issueTimestamp"  -> JsNumber(timestamp),
        "issuer"          -> JsString(description.issuer.toAddress.toString),
        "issuerPublicKey" -> JsString(description.issuer.toString),
        "name"            -> JsString(name),
        "description"     -> JsString(desc),
        "decimals"        -> JsNumber(description.decimals),
        "reissuable"      -> JsBoolean(description.reissuable),
        "quantity"        -> JsNumber(BigDecimal(description.totalVolume)),
        "scripted"        -> JsBoolean(description.script.nonEmpty),
        "minSponsoredAssetFee" -> (description.sponsorship match {
          case 0           => JsNull
          case sponsorship => JsNumber(sponsorship)
        }),
        "originTransactionId" -> JsString(description.originTransactionId.toString)
      ) ++ script.toSeq.map {
        case AssetScriptInfo(script, complexity) =>
          "scriptDetails" -> Json.obj(
            "scriptComplexity" -> JsNumber(BigDecimal(complexity)),
            "script"           -> JsString(script.bytes().base64),
            "scriptText"       -> JsString(script.expr.toString) // [WAIT] JsString(Script.decompile(script))
          )
      }
    )
  }
}

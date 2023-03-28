package com.wavesplatform.api.http.assets

import akka.NotUsed
import akka.http.scaladsl.marshalling.{ToResponseMarshallable, ToResponseMarshaller}
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.Source
import cats.data.Validated
import cats.instances.either.*
import cats.instances.list.*
import cats.syntax.alternative.*
import cats.syntax.either.*
import cats.syntax.traverse.*
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.{CommonAccountsApi, CommonAssetsApi}
import com.wavesplatform.api.http.*
import com.wavesplatform.api.http.ApiError.*
import com.wavesplatform.api.http.assets.AssetsApiRoute.{AssetDetails, AssetInfo, DistributionParams, assetDetailsCodec}
import com.wavesplatform.api.http.requests.*
import com.wavesplatform.api.http.StreamSerializerUtils.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.state.{AssetDescription, AssetScriptInfo, Blockchain}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.EthereumTransaction.Invocation
import com.wavesplatform.transaction.{EthereumTransaction, TransactionFactory, TxTimestamp, TxVersion}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.smart.{InvokeExpressionTransaction, InvokeScriptTransaction}
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import io.netty.util.concurrent.DefaultThreadFactory
import monix.execution.Scheduler
import monix.reactive.Observable
import play.api.libs.json.*

import java.util.concurrent.*
import scala.concurrent.Future

case class AssetsApiRoute(
    settings: RestAPISettings,
    wallet: Wallet,
    transactionPublisher: TransactionPublisher,
    blockchain: Blockchain,
    compositeBlockchain: () => CompositeBlockchain,
    time: Time,
    commonAccountApi: CommonAccountsApi,
    commonAssetsApi: CommonAssetsApi,
    maxDistributionDepth: Int,
    routeTimeout: RouteTimeout
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

  private def deprecatedRoute: Route = {
    post {
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
    }
  }

  override lazy val route: Route =
    pathPrefix("assets") {
      pathPrefix("balance" / AddrSegment) { address =>
        anyParam("id", limit = settings.assetDetailsLimit) { assetIds =>
          val assetIdsValidated = assetIds.toList
            .map(assetId => ByteStr.decodeBase58(assetId).fold(_ => Left(assetId), bs => Right(IssuedAsset(bs))).toValidatedNel)
            .sequence

          assetIdsValidated match {
            case Validated.Valid(assets) =>
              balances(address, Some(assets).filter(_.nonEmpty))

            case Validated.Invalid(invalidAssets) =>
              complete(InvalidIds(invalidAssets.toList))
          }
        } ~ (get & path(AssetId)) { assetId =>
          balance(address, assetId)
        }
      } ~ pathPrefix("details") {
        (anyParam("id", limit = settings.assetDetailsLimit) & parameter("full".as[Boolean] ? false)) { (ids, full) =>
          val result = Either
            .cond(ids.nonEmpty, (), AssetIdNotSpecified)
            .map(_ => multipleDetails(ids.toList, full))

          complete(result)
        } ~ (get & path(AssetId) & parameter("full".as[Boolean] ? false)) { (assetId, full) =>
          singleDetails(assetId, full)
        }
      } ~ get {
        (path("nft" / AddrSegment / "limit" / IntNumber) & parameter("after".as[String].?)) { (address, limit, maybeAfter) =>
          nft(address, limit, maybeAfter)
        } ~ pathPrefix(AssetId / "distribution") { assetId =>
          pathEndOrSingleSlash(balanceDistribution(assetId)) ~
            (path(IntNumber / "limit" / IntNumber) & parameter("after".?)) { (height, limit, maybeAfter) =>
              balanceDistributionAtHeight(assetId, height, limit, maybeAfter)
            }
        }
      } ~ deprecatedRoute
    }

  private def multipleDetails(ids: List[String], full: Boolean): ToResponseMarshallable =
    ids.map(id => ByteStr.decodeBase58(id).toEither.leftMap(_ => id)).separate match {
      case (Nil, assetIds) =>
        assetIds.map(id => assetDetails(IssuedAsset(id), full)).separate match {
          case (Nil, details) => details
          case (errors, _) =>
            val notFoundErrors = errors.collect { case AssetDoesNotExist(assetId) => assetId }
            if (notFoundErrors.isEmpty) {
              errors.head
            } else {
              AssetsDoesNotExist(notFoundErrors)
            }
        }
      case (errors, _) => InvalidIds(errors)
    }

  def getFullAssetInfo(balances: Seq[(IssuedAsset, Long)]): Seq[AssetInfo] =
    balances.view
      .zip(commonAssetsApi.fullInfos(balances.map(_._1)))
      .map { case ((asset, balance), infoOpt) =>
        infoOpt match {
          case Some(CommonAssetsApi.AssetInfo(assetInfo, issueTransaction, sponsorBalance)) =>
            AssetInfo.FullAssetInfo(
              assetId = asset.id.toString,
              reissuable = assetInfo.reissuable,
              minSponsoredAssetFee = assetInfo.sponsorship match {
                case 0           => None
                case sponsorship => Some(sponsorship)
              },
              sponsorBalance = sponsorBalance,
              quantity = BigDecimal(assetInfo.totalVolume),
              issueTransaction = issueTransaction,
              balance = balance,
              sequenceInBlock = assetInfo.sequenceInBlock
            )
          case None => AssetInfo.AssetId(asset.id.toString)
        }
      }
      .toSeq

  /** @param assets
    *   Some(assets) for specific asset balances, None for a full portfolio
    */
  def balances(address: Address, assets: Option[Seq[IssuedAsset]] = None): Route = {
    implicit val jsonStreamingSupport: ToResponseMarshaller[Source[AssetInfo, NotUsed]] =
      jsonBytesStreamMarshaller(s"""{"address":"$address","balances":[""", ",", "]}")(AssetsApiRoute.assetInfoCodec)

    routeTimeout.executeFromObservable(
      (assets match {
        case Some(assets) =>
          Observable.eval(assets.map(asset => asset -> blockchain.balance(address, asset)))
        case None =>
          commonAccountApi
            .portfolio(address)
      }).concatMapIterable(getFullAssetInfo)
    )
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
      Json.toJson(l.map { case (a, b) => a.toString -> b }.toMap)
    }

  def balanceDistributionAtHeight(assetId: IssuedAsset, heightParam: Int, limitParam: Int, afterParam: Option[String]): Route =
    optionalHeaderValueByType(Accept) { accept =>
      val paramsEi: Either[ValidationError, DistributionParams] =
        AssetsApiRoute
          .validateDistributionParams(blockchain, heightParam, limitParam, settings.distributionAddressLimit, afterParam, maxDistributionDepth)

      paramsEi match {
        case Right((height, limit, after)) =>
          balanceDistribution(assetId, height, limit, after) { l =>
            Json.obj(
              "hasNext"  -> (l.length == limit),
              "lastItem" -> l.lastOption.map(_._1),
              "items" -> Json.toJson(l.map { case (a, b) =>
                a.toString -> accept.fold[JsValue](JsNumber(b)) {
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

  def nft(address: Address, limit: Int, maybeAfter: Option[String]): Route = {
    val after = maybeAfter.collect { case s if s.nonEmpty => IssuedAsset(ByteStr.decodeBase58(s).getOrElse(throw ApiException(InvalidAssetId))) }
    if (limit > settings.transactionsByAddressLimit) complete(TooBigArrayAllocation)
    else {
      import cats.syntax.either.*
      implicit val jsonStreamingSupport: ToResponseMarshaller[Source[AssetDetails, NotUsed]] = jsonBytesStreamMarshaller()(assetDetailsCodec)

      val compBlockchain = compositeBlockchain()
      routeTimeout.executeStreamed {
        commonAccountApi
          .nftList(address, after)
          .concatMapIterable { a =>
            AssetsApiRoute
              .getAssetDetails(compBlockchain)(a, full = true)
              .valueOr(err => throw new IllegalArgumentException(err))
          }
          .take(limit)
          .toListL
      }(identity)
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

  def getAssetDetails(blockchain: Blockchain)(assets: Seq[(IssuedAsset, AssetDescription)], full: Boolean): Either[String, Seq[AssetDetails]] = {
    def getTimestamps(ids: Seq[ByteStr]): Either[String, Seq[TxTimestamp]] = {
      blockchain.transactionInfos(ids).traverse { infoOpt =>
        for {
          (_, tx) <- infoOpt
            .filter { case (tm, _) => tm.succeeded }
            .toRight("Failed to find issue/invokeScript/invokeExpression transaction by ID")
          ts <- (tx match {
            case tx: IssueTransaction                             => Some(tx.timestamp)
            case tx: InvokeScriptTransaction                      => Some(tx.timestamp)
            case tx: InvokeExpressionTransaction                  => Some(tx.timestamp)
            case tx @ EthereumTransaction(_: Invocation, _, _, _) => Some(tx.timestamp)
            case _                                                => None
          }).toRight("No issue/invokeScript/invokeExpression transaction found with the given asset ID")
        } yield ts
      }
    }

    getTimestamps(assets.map { case (_, description) => description.originTransactionId }).map { infos =>
      assets.zip(infos).map { case ((id, description), timestamp) =>
        AssetDetails(
          assetId = id.id.toString,
          issueHeight = description.issueHeight,
          issueTimestamp = timestamp,
          issuer = description.issuer.toAddress.toString,
          issuerPublicKey = description.issuer.toString,
          name = description.name.toStringUtf8,
          description = description.description.toStringUtf8,
          decimals = description.decimals,
          reissuable = description.reissuable,
          quantity = BigDecimal(description.totalVolume),
          scripted = description.script.nonEmpty,
          minSponsoredAssetFee = description.sponsorship match {
            case 0           => None
            case sponsorship => Some(sponsorship)
          },
          originTransactionId = description.originTransactionId.toString,
          sequenceInBlock = description.sequenceInBlock,
          scriptDetails = description.script.filter(_ => full).map { case AssetScriptInfo(script, complexity) =>
            AssetScriptDetails(
              scriptComplexity = BigDecimal(complexity),
              script = script.bytes().base64,
              scriptText = script.expr.toString // [WAIT] Script.decompile(script)
            )
          }
        )
      }
    }
  }

  def jsonDetails(blockchain: Blockchain)(id: IssuedAsset, description: AssetDescription, full: Boolean): Either[String, JsObject] = {
    // (timestamp, height)
    def additionalInfo(id: ByteStr): Either[String, Long] =
      for {
        (_, tx) <- blockchain
          .transactionInfo(id)
          .filter { case (tm, _) => tm.succeeded }
          .toRight("Failed to find issue/invokeScript/invokeExpression transaction by ID")
        timestamp <- (tx match {
          case tx: IssueTransaction                             => Some(tx.timestamp)
          case tx: InvokeScriptTransaction                      => Some(tx.timestamp)
          case tx: InvokeExpressionTransaction                  => Some(tx.timestamp)
          case tx @ EthereumTransaction(_: Invocation, _, _, _) => Some(tx.timestamp)
          case _                                                => None
        }).toRight("No issue/invokeScript/invokeExpression transaction found with the given asset ID")
      } yield timestamp

    for {
      timestamp <- additionalInfo(description.originTransactionId)
      script = description.script.filter(_ => full)
      name   = description.name.toStringUtf8
      desc   = description.description.toStringUtf8
    } yield JsObject(
      Seq(
        "assetId"         -> JsString(id.id.toString),
        "issueHeight"     -> JsNumber(description.issueHeight),
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
        "originTransactionId" -> JsString(description.originTransactionId.toString),
        "sequenceInBlock"     -> JsNumber(description.sequenceInBlock)
      ) ++ script.toSeq.map { case AssetScriptInfo(script, complexity) =>
        "scriptDetails" -> Json.obj(
          "scriptComplexity" -> JsNumber(BigDecimal(complexity)),
          "script"           -> JsString(script.bytes().base64),
          "scriptText"       -> JsString(script.expr.toString) // [WAIT] JsString(Script.decompile(script))
        )
      }
    )
  }

  case class AssetScriptDetails(
      scriptComplexity: BigDecimal,
      script: String,
      scriptText: String
  )

  case class AssetDetails(
      assetId: String,
      issueHeight: Int,
      issueTimestamp: Long,
      issuer: String,
      issuerPublicKey: String,
      name: String,
      description: String,
      decimals: Int,
      reissuable: Boolean,
      quantity: BigDecimal,
      scripted: Boolean,
      minSponsoredAssetFee: Option[Long],
      originTransactionId: String,
      sequenceInBlock: Int,
      scriptDetails: Option[AssetScriptDetails]
  )

  sealed trait AssetInfo
  object AssetInfo {
    case class FullAssetInfo(
        assetId: String,
        reissuable: Boolean,
        minSponsoredAssetFee: Option[Long],
        sponsorBalance: Option[Long],
        quantity: BigDecimal,
        issueTransaction: Option[IssueTransaction],
        balance: Long,
        sequenceInBlock: Int
    ) extends AssetInfo

    case class AssetId(assetId: String) extends AssetInfo
  }

  implicit val assetIdCodec: JsonValueCodec[AssetInfo.AssetId] = JsonCodecMaker.make

  def assetScriptDetailsCodec(numbersAsString: Boolean): JsonValueCodec[AssetScriptDetails] = new OnlyEncodeJsonValueCodec[AssetScriptDetails] {
    override def encodeValue(details: AssetScriptDetails, out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKeyValue("scriptComplexity", details.scriptComplexity, numbersAsString)
      out.writeKeyValue("script", details.script)
      out.writeKeyValue("scriptText", details.scriptText)
      out.writeObjectEnd()
    }
  }

  def assetDetailsCodec(numbersAsString: Boolean): JsonValueCodec[AssetDetails] = new OnlyEncodeJsonValueCodec[AssetDetails] {
    override def encodeValue(details: AssetDetails, out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKeyValue("assetId", details.assetId)
      out.writeKeyValue("issueHeight", details.issueHeight, numbersAsString)
      out.writeKeyValue("issueTimestamp", details.issueTimestamp, numbersAsString)
      out.writeKeyValue("issuer", details.issuer)
      out.writeKeyValue("issuerPublicKey", details.issuerPublicKey)
      out.writeKeyValue("name", details.name)
      out.writeKeyValue("description", details.description)
      out.writeKeyValue("decimals", details.decimals, numbersAsString)
      out.writeKeyValue("reissuable", details.reissuable)
      out.writeKeyValue("quantity", details.quantity, numbersAsString)
      out.writeKeyValue("scripted", details.scripted)
      details.minSponsoredAssetFee.foreach(fee => out.writeKeyValue("minSponsoredAssetFee", fee, numbersAsString))
      out.writeKeyValue("originTransactionId", details.originTransactionId)
      out.writeKeyValue("sequenceInBlock", details.sequenceInBlock, numbersAsString)
      details.scriptDetails.foreach(sd => out.writeKeyValue("scriptDetails", assetScriptDetailsCodec(numbersAsString).encodeValue(sd, _)))
      out.writeObjectEnd()
    }
  }

  def issueTxCodec(numbersAsString: Boolean): JsonValueCodec[IssueTransaction] = new OnlyEncodeJsonValueCodec[IssueTransaction] {
    override def encodeValue(tx: IssueTransaction, out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKeyValue("type", tx.tpe.id, numbersAsString)
      out.writeKeyValue("id", tx.id().toString)
      out.writeKeyValue("fee", tx.assetFee._2, numbersAsString)
      tx.assetFee._1.maybeBase58Repr.fold(out.writeKeyNull("feeAssetId"))(out.writeKeyValue("feeAssetId", _))
      out.writeKeyValue("timestamp", tx.timestamp, numbersAsString)
      out.writeKeyValue("version", tx.version, numbersAsString)
      if (tx.version >= TxVersion.V2) out.writeKeyValue("chainId", tx.chainId, numbersAsString) else out.writeKeyNull("chainId")
      out.writeKeyValue("sender", tx.sender.toAddress(tx.chainId).toString)
      out.writeKeyValue("senderPublicKey", tx.sender.toString)
      out.writeKeyValueArray("proofs", out => tx.proofs.proofs.foreach(p => out.writeVal(p.toString)))
      out.writeKeyValue("assetId", tx.assetId.toString)
      out.writeKeyValue("name", tx.name.toStringUtf8)
      out.writeKeyValue("quantity", tx.quantity.value, numbersAsString)
      out.writeKeyValue("reissuable", tx.reissuable)
      out.writeKeyValue("decimals", tx.decimals.value, numbersAsString)
      out.writeKeyValue("description", tx.description.toStringUtf8)
      if (tx.version >= TxVersion.V2) {
        tx.script.map(_.bytes().base64).fold(out.writeKeyNull("script"))(out.writeKeyValue("script", _))
      }
      if (tx.usesLegacySignature) out.writeKeyValue("signature", tx.signature.toString)
      out.writeObjectEnd()
    }
  }

  def assetInfoCodec(numbersAsString: Boolean): JsonValueCodec[AssetInfo] = new OnlyEncodeJsonValueCodec[AssetInfo] {
    override def encodeValue(x: AssetInfo, out: JsonWriter): Unit = x match {
      case info: AssetInfo.FullAssetInfo =>
        out.writeObjectStart()
        out.writeKeyValue("assetId", info.assetId)
        out.writeKeyValue("reissuable", info.reissuable)
        info.minSponsoredAssetFee.foreach(out.writeKeyValue("minSponsoredAssetFee", _, numbersAsString))
        info.sponsorBalance.foreach(out.writeKeyValue("sponsorBalance", _, numbersAsString))
        out.writeKeyValue("quantity", info.quantity, numbersAsString)
        info.issueTransaction.foreach(tx => out.writeKeyValue("issueTransaction", issueTxCodec(numbersAsString).encodeValue(tx, _)))
        out.writeKeyValue("balance", info.balance, numbersAsString)
        out.writeKeyValue("sequenceInBlock", info.sequenceInBlock, numbersAsString)
        out.writeObjectEnd()
      case assetId: AssetInfo.AssetId => assetIdCodec.encodeValue(assetId, out)
    }
  }
}

package com.wavesplatform.matcher.api

import akka.actor.ActorRef
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Directive0, Directive1, Route}
import akka.pattern.ask
import akka.util.Timeout
import com.google.common.primitives.Longs
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.http._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.matcher.AddressActor.GetOrderStatus
import com.wavesplatform.matcher.AddressDirectory.{Envelope => Env}
import com.wavesplatform.matcher.Matcher.StoreEvent
import com.wavesplatform.matcher.error.MatcherError
import com.wavesplatform.matcher.error.MatcherError.OrderRestrictionsNotFound
import com.wavesplatform.matcher.market.MatcherActor.{ForceStartOrderBook, GetMarkets, GetSnapshotOffsets, MarketData, SnapshotOffsetsResponse}
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model._
import com.wavesplatform.matcher.queue.{QueueEvent, QueueEventWithMeta}
import com.wavesplatform.matcher.settings.MatcherSettings
import com.wavesplatform.matcher.{AddressActor, AssetPairBuilder, Matcher, RateCache}
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.OrderJson._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.{ScorexLogging, Time}
import io.swagger.annotations._
import javax.ws.rs.Path
import kamon.Kamon
import org.iq80.leveldb.DB
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

@Path("/matcher")
@Api(value = "/matcher/")
case class MatcherApiRoute(assetPairBuilder: AssetPairBuilder,
                           matcherPublicKey: PublicKey,
                           matcher: ActorRef,
                           addressActor: ActorRef,
                           storeEvent: StoreEvent,
                           orderBook: AssetPair => Option[Either[Unit, ActorRef]],
                           getMarketStatus: AssetPair => Option[MarketStatus],
                           orderValidator: Order => Either[MatcherError, Order],
                           orderBookSnapshot: OrderBookSnapshotHttpCache,
                           matcherSettings: MatcherSettings,
                           matcherStatus: () => Matcher.Status,
                           db: DB,
                           time: Time,
                           currentOffset: () => QueueEventWithMeta.Offset,
                           lastOffset: () => Future[QueueEventWithMeta.Offset],
                           matcherAccountFee: Long,
                           apiKeyHash: Option[Array[Byte]],
                           rateCache: RateCache,
                           validatedAllowedOrderVersions: Set[Byte])
    extends ApiRoute
    with ScorexLogging {

  import MatcherApiRoute._
  import PathMatchers._

  private implicit val timeout: Timeout = matcherSettings.actorResponseTimeout

  private val timer      = Kamon.timer("matcher.api-requests")
  private val placeTimer = timer.refine("action" -> "place")

  override lazy val route: Route = pathPrefix("matcher") {
    matcherStatusBarrier {
      getMatcherPublicKey ~ getOrderBook ~ marketStatus ~ orderRestrictionsInfo ~ place ~ getAssetPairAndPublicKeyOrderHistory ~ getPublicKeyOrderHistory ~
        getAllOrderHistory ~ tradableBalance ~ reservedBalance ~ orderStatus ~
        historyDelete ~ cancel ~ cancelAll ~ orderbooks ~ orderBookDelete ~ getTransactionsByOrder ~ forceCancelOrder ~
        getSettings ~ getRates ~ upsertRate ~ deleteRate ~ getCurrentOffset ~ getLastOffset ~ getOldestSnapshotOffset ~ getAllSnapshotOffsets
    }
  }

  private def wrapMessage(message: String): JsObject = Json.obj("message" -> message)

  private def matcherStatusBarrier: Directive0 = matcherStatus() match {
    case Matcher.Status.Working  => pass
    case Matcher.Status.Starting => complete(DuringStart)
    case Matcher.Status.Stopping => complete(DuringShutdown)
  }

  private def unavailableOrderBookBarrier(p: AssetPair): Directive0 = orderBook(p) match {
    case Some(x) => if (x.isRight) pass else complete(OrderBookUnavailable(MatcherError.OrderBookUnavailable(p)))
    case None    => forceCheckOrderBook(p)
  }

  private def forceCheckOrderBook(p: AssetPair): Directive0 = onComplete(matcher ? ForceStartOrderBook(p)).flatMap {
    case Success(_) => pass
    case _          => complete(OrderBookUnavailable(MatcherError.OrderBookUnavailable(p)))
  }

  private def withAssetPair(p: AssetPair, redirectToInverse: Boolean = false, suffix: String = ""): Directive1[AssetPair] =
    assetPairBuilder.validateAssetPair(p) match {
      case Right(_) => provide(p)
      case Left(e) if redirectToInverse =>
        assetPairBuilder
          .validateAssetPair(p.reverse)
          .fold(
            _ => complete(StatusCodes.NotFound -> Json.obj("message" -> e)),
            _ => redirect(s"/matcher/orderbook/${p.priceAssetStr}/${p.amountAssetStr}$suffix", StatusCodes.MovedPermanently)
          )
      case Left(e) => complete(StatusCodes.NotFound -> Json.obj("message" -> e))
    }

  private def withAsset(a: Asset): Directive1[Asset] = {
    assetPairBuilder.validateAssetId(a) match {
      case Right(_) => provide(a)
      case Left(e)  => complete(StatusCodes.NotFound -> Json.obj("message" -> e))
    }
  }

  private def withCancelRequest(f: CancelOrderRequest => Route): Route =
    post {
      entity(as[CancelOrderRequest]) { req =>
        if (req.isSignatureValid()) f(req) else complete(CancelRequestInvalidSignature)
      } ~ complete(StatusCodes.BadRequest)
    } ~ complete(StatusCodes.MethodNotAllowed)

  private def signedGet(publicKey: PublicKey): Directive0 =
    (headerValueByName("Timestamp") & headerValueByName("Signature")).tflatMap {
      case (timestamp, sig) =>
        require(crypto.verify(Base58.tryDecodeWithLimit(sig).get, publicKey ++ Longs.toByteArray(timestamp.toLong), publicKey), "Incorrect signature")
        pass
    }

  @inline
  private def askAddressActor[A: ClassTag](sender: Address, msg: AddressActor.Command): Future[A] = {
    (addressActor ? Env(sender, msg))
      .mapTo[A]
      .andThen {
        case Failure(e) => log.warn(s"Error processing $msg", e)
      }
  }

  @Path("/")
  @ApiOperation(value = "Matcher Public Key", notes = "Get matcher public key", httpMethod = "GET")
  def getMatcherPublicKey: Route = (pathEndOrSingleSlash & get) {
    complete(JsString(Base58.encode(matcherPublicKey)))
  }

  @Path("/settings")
  @ApiOperation(value = "Matcher Settings", notes = "Get matcher settings", httpMethod = "GET")
  def getSettings: Route = (path("settings") & get) {
    complete(
      StatusCodes.OK -> Json.obj(
        "priceAssets"   -> matcherSettings.priceAssets,
        "orderFee"      -> matcherSettings.orderFee.getJson(matcherAccountFee, rateCache.getJson).value,
        "orderVersions" -> validatedAllowedOrderVersions.toSeq.sorted
      )
    )
  }

  @Path("/settings/rates")
  @ApiOperation(value = "Asset rates", notes = "Get current rates of assets (asset cost in Waves)", httpMethod = "GET")
  def getRates: Route = (path("settings" / "rates") & get) { complete(StatusCodes.OK -> rateCache.getJson) }

  @Path("/settings/rates/{assetId}")
  @ApiOperation(value = "Add or update rate for the specified asset", httpMethod = "PUT")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "assetId", value = "Asset for which rate is added or updated", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "rate",
                           value = "Rate associated with the specified asset",
                           dataType = "double",
                           paramType = "body",
                           required = true)
    )
  )
  def upsertRate: Route = (path("settings" / "rates" / AssetPM) & put & withAuth) { a =>
    entity(as[Double]) { rate =>
      withAsset(a) { asset =>
        complete(
          if (asset == Waves) StatusCodes.BadRequest -> wrapMessage("Rate for Waves cannot be changed")
          else {
            val assetStr = AssetPair.assetIdStr(asset)
            rateCache.upsertRate(asset, rate) match {
              case None     => StatusCodes.Created -> wrapMessage(s"Rate $rate for the asset $assetStr added")
              case Some(pv) => StatusCodes.OK      -> wrapMessage(s"Rate for the asset $assetStr updated, old value = $pv, new value = $rate")
            }
          }
        )
      }
    }
  }

  @Path("/settings/rates/{assetId}")
  @ApiOperation(value = "Delete rate for the specified asset", httpMethod = "DELETE")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "assetId", value = "Asset for which rate is deleted", dataType = "string", paramType = "path")
    )
  )
  def deleteRate: Route = (path("settings" / "rates" / AssetPM) & delete  & withAuth) { a =>
    withAsset(a) { asset =>
      complete(
        if (asset == Waves) StatusCodes.BadRequest -> wrapMessage("Rate for Waves cannot be deleted")
        else {
          val assetStr = AssetPair.assetIdStr(asset)
          rateCache.deleteRate(asset) match {
            case None     => StatusCodes.NotFound -> wrapMessage(s"Rate for the asset $assetStr is not specified")
            case Some(pv) => StatusCodes.OK       -> wrapMessage(s"Rate for the asset $assetStr deleted, old value = $pv")
          }
        }
      )
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}")
  @ApiOperation(value = "Get Order Book for a given Asset Pair", notes = "Get Order Book for a given Asset Pair", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "depth",
                           value = "Limit the number of bid/ask records returned",
                           required = false,
                           dataType = "integer",
                           paramType = "query")
    ))
  def getOrderBook: Route = (path("orderbook" / AssetPairPM) & get) { p =>
    parameters('depth.as[Int].?) { depth =>
      withAssetPair(p, redirectToInverse = true) { pair =>
        complete(orderBookSnapshot.get(pair, depth))
      }
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}/status")
  @ApiOperation(value = "Get Market Status", notes = "Get current market data such as last trade, best bid and ask", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path")
    ))
  def marketStatus: Route = (path("orderbook" / AssetPairPM / "status") & get) { p =>
    withAssetPair(p, redirectToInverse = true) { pair =>
      getMarketStatus(pair).fold(complete(StatusCodes.NotFound -> Json.obj("message" -> "There is no information about this asset pair"))) { ms =>
        complete(StatusCodes.OK -> ms)
      }
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}/info")
  @ApiOperation(value = "Get order restrictions for the specified asset pair", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path")
    ))
  def orderRestrictionsInfo: Route = (path("orderbook" / AssetPairPM / "info") & get) { p =>
    withAssetPair(p, redirectToInverse = true, suffix = "/info") { pair =>
      matcherSettings.orderRestrictions
        .get(pair)
        .fold { complete(InfoNotFound(OrderRestrictionsNotFound(pair))) } { restrictions =>
          complete(StatusCodes.OK -> restrictions.getJson.value)
        }
    }
  }

  @Path("/orderbook")
  @ApiOperation(value = "Place order",
                notes = "Place a new limit order (buy or sell)",
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
        dataType = "com.wavesplatform.transaction.assets.exchange.Order"
      )
    ))
  def place: Route = path("orderbook") {
    (pathEndOrSingleSlash & post & jsonEntity[Order]) { order =>
      unavailableOrderBookBarrier(order.assetPair) {
        complete(placeTimer.measureFuture(orderValidator(order) match {
          case Right(_)    => placeTimer.measureFuture(askAddressActor[MatcherResponse](order.sender, AddressActor.PlaceOrder(order)))
          case Left(error) => Future.successful[MatcherResponse](OrderRejected(error))
        }))
      }
    }
  }

  @Path("/orderbook")
  @ApiOperation(value = "Get the open trading markets", notes = "Get the open trading markets along with trading pairs meta data", httpMethod = "GET")
  def orderbooks: Route = (path("orderbook") & pathEndOrSingleSlash & get) {
    complete((matcher ? GetMarkets).mapTo[Seq[MarketData]].map { markets =>
      StatusCodes.OK -> Json.obj(
        "matcherPublicKey" -> Base58.encode(matcherPublicKey),
        "markets" -> JsArray(markets.map(m =>
          Json.obj(
            "amountAsset"     -> m.pair.amountAssetStr,
            "amountAssetName" -> m.amountAssetName,
            "amountAssetInfo" -> m.amountAssetInfo,
            "priceAsset"      -> m.pair.priceAssetStr,
            "priceAssetName"  -> m.priceAssetName,
            "priceAssetInfo"  -> m.priceAssetinfo,
            "created"         -> m.created,
            "restrictions"    -> matcherSettings.orderRestrictions.get(m.pair).map(_.getJson.value)
        )))
      )
    })
  }

  private def handleCancelRequest(assetPair: Option[AssetPair], sender: Address, orderId: Option[ByteStr], timestamp: Option[Long]): Route =
    complete((timestamp, orderId) match {
      case (Some(ts), None)  => askAddressActor[MatcherResponse](sender, AddressActor.CancelAllOrders(assetPair, ts))
      case (None, Some(oid)) => askAddressActor[MatcherResponse](sender, AddressActor.CancelOrder(oid))
      case _                 => OrderCancelRejected(MatcherError.CancelRequestIsIncomplete)
    })

  private def handleCancelRequest(assetPair: Option[AssetPair]): Route =
    withCancelRequest { req =>
      assetPair.fold(pass)(unavailableOrderBookBarrier).apply {
        handleCancelRequest(assetPair, req.sender, req.orderId, req.timestamp)
      }
    }

  @Path("/orderbook/{amountAsset}/{priceAsset}/cancel")
  @ApiOperation(
    value = "Cancel order",
    notes = "Cancel previously submitted order if it's not already filled completely",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json"
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.matcher.api.CancelOrderRequest"
      )
    ))
  def cancel: Route = (path("orderbook" / AssetPairPM / "cancel") & post) { p =>
    unavailableOrderBookBarrier(p) {
      withAssetPair(p) { pair =>
        handleCancelRequest(Some(pair))
      }
    }
  }

  @Path("/orderbook/cancel")
  @ApiOperation(
    value = "Cancel all active orders",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json"
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.matcher.api.CancelOrderRequest"
      )
    ))
  def cancelAll: Route = (path("orderbook" / "cancel") & post) {
    handleCancelRequest(None)
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}/delete")
  @Deprecated
  @ApiOperation(
    value = "Delete Order from History by Id",
    notes = "This method is deprecated and doesn't work anymore. Please don't use it.",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json"
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.matcher.api.CancelOrderRequest"
      )
    ))
  def historyDelete: Route = (path("orderbook" / AssetPairPM / "delete") & post) { _ =>
    json[CancelOrderRequest] { req =>
      req.orderId.fold[MatcherResponse](NotImplemented(MatcherError.FeatureNotImplemented))(OrderDeleted)
    }
  }

  private def loadOrders(address: Address, pair: Option[AssetPair], activeOnly: Boolean): Route = complete {
    askAddressActor[Seq[(ByteStr, OrderInfo[OrderStatus])]](address, AddressActor.GetOrders(pair, activeOnly))
      .map(orders =>
        StatusCodes.OK -> orders.map {
          case (id, oi) =>
            Json.obj(
              "id" -> id.toString,
              "type"      -> oi.side.toString,
              "amount"    -> oi.amount,
              "price"     -> oi.price,
              "timestamp" -> oi.timestamp,
              "filled" -> (oi.status match {
                case OrderStatus.Filled(f)          => f
                case OrderStatus.PartiallyFilled(f) => f
                case OrderStatus.Cancelled(f)       => f
                case _                              => 0L
              }),
              "status"    -> oi.status.name,
              "assetPair" -> oi.assetPair.json
            )
      })
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}/publicKey/{publicKey}")
  @ApiOperation(value = "Order History by Asset Pair and Public Key",
                notes = "Get Order History for a given Asset Pair and Public Key",
                httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "publicKey", value = "Public Key", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "activeOnly",
        value = "Return active only orders (Accepted and PartiallyFilled)",
        required = false,
        dataType = "boolean",
        paramType = "query",
        defaultValue = "false"
      ),
      new ApiImplicitParam(name = "Timestamp", value = "Timestamp", required = true, dataType = "integer", paramType = "header"),
      new ApiImplicitParam(name = "Signature",
                           value = "Signature of [Public Key ++ Timestamp] bytes",
                           required = true,
                           dataType = "string",
                           paramType = "header")
    ))
  def getAssetPairAndPublicKeyOrderHistory: Route = (path("orderbook" / AssetPairPM / "publicKey" / PublicKeyPM) & get) { (p, publicKey) =>
    withAssetPair(p, redirectToInverse = true, s"/publicKey/$publicKey") { pair =>
      parameters('activeOnly.as[Boolean].?) { activeOnly =>
        signedGet(publicKey) {
          loadOrders(publicKey, Some(pair), activeOnly.getOrElse(false))
        }
      }
    }
  }

  @Path("/orderbook/{publicKey}")
  @ApiOperation(value = "Order History by Public Key", notes = "Get Order History for a given Public Key", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "publicKey", value = "Public Key", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "activeOnly",
        value = "Return active only orders (Accepted and PartiallyFilled)",
        required = false,
        dataType = "boolean",
        paramType = "query",
        defaultValue = "false"
      ),
      new ApiImplicitParam(name = "Timestamp", value = "Timestamp", required = true, dataType = "integer", paramType = "header"),
      new ApiImplicitParam(name = "Signature",
                           value = "Signature of [Public Key ++ Timestamp] bytes",
                           required = true,
                           dataType = "string",
                           paramType = "header")
    ))
  def getPublicKeyOrderHistory: Route = (path("orderbook" / PublicKeyPM) & get) { publicKey =>
    parameters('activeOnly.as[Boolean].?) { activeOnly =>
      signedGet(publicKey) {
        loadOrders(publicKey, None, activeOnly.getOrElse(false))
      }
    }
  }

  @Path("/orders/cancel/{orderId}")
  @ApiOperation(value = "Cancel Order by ID without signature", notes = "Cancel Order by ID without signature", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "orderId", value = "Order Id", required = true, dataType = "string", paramType = "path")
    ))
  def forceCancelOrder: Route = (path("orders" / "cancel" / ByteStrPM) & post & withAuth) { orderId =>
    DBUtils.order(db, orderId) match {
      case Some(order) => handleCancelRequest(None, order.sender, Some(orderId), None)
      case None        => complete(OrderCancelRejected(MatcherError.OrderNotFound(orderId)))
    }
  }

  @Path("/orders/{address}")
  @ApiOperation(value = "All Order History by address", notes = "Get All Order History for a given address", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "activeOnly",
        value = "Return active only orders (Accepted and PartiallyFilled)",
        required = false,
        dataType = "boolean",
        paramType = "query",
        defaultValue = "false"
      ),
    ))
  def getAllOrderHistory: Route = (path("orders" / AddressPM) & get & withAuth) { address =>
    parameters('activeOnly.as[Boolean].?) { activeOnly =>
      loadOrders(address, None, activeOnly.getOrElse(true))
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}/tradableBalance/{address}")
  @ApiOperation(value = "Tradable balance for Asset Pair", notes = "Get Tradable balance for the given Asset Pair", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "address", value = "Account Address", required = true, dataType = "string", paramType = "path")
    ))
  def tradableBalance: Route = (path("orderbook" / AssetPairPM / "tradableBalance" / AddressPM) & get) { (pair, address) =>
    withAssetPair(pair, redirectToInverse = true, s"/tradableBalance/$address") { pair =>
      complete {
        askAddressActor[Map[Asset, Long]](address, AddressActor.GetTradableBalance(pair))
          .map(stringifyAssetIds)
      }
    }
  }

  @Path("/balance/reserved/{publicKey}")
  @ApiOperation(value = "Reserved Balance", notes = "Get non-zero balance of open orders", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "publicKey", value = "Public Key", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "Timestamp", value = "Timestamp", required = true, dataType = "integer", paramType = "header"),
      new ApiImplicitParam(name = "Signature",
                           value = "Signature of [Public Key ++ Timestamp] bytes",
                           required = true,
                           dataType = "string",
                           paramType = "header")
    ))
  def reservedBalance: Route = (path("balance" / "reserved" / PublicKeyPM) & get) { publicKey =>
    signedGet(publicKey) {
      complete {
        askAddressActor[Map[Asset, Long]](publicKey, AddressActor.GetReservedBalance)
          .map(stringifyAssetIds)
      }
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}/{orderId}")
  @ApiOperation(value = "Order Status", notes = "Get Order status for a given Asset Pair during the last 30 days", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "orderId", value = "Order Id", required = true, dataType = "string", paramType = "path")
    ))
  def orderStatus: Route = (path("orderbook" / AssetPairPM / ByteStrPM) & get) { (p, orderId) =>
    withAssetPair(p, redirectToInverse = true, s"/$orderId") { _ =>
      complete(
        DBUtils
          .order(db, orderId)
          .fold[Future[OrderStatus]](Future.successful(OrderStatus.NotFound)) { order =>
            askAddressActor[OrderStatus](order.sender, GetOrderStatus(orderId))
          }
          .map(_.json))
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}")
  @ApiOperation(
    value = "Remove Order Book for a given Asset Pair",
    notes = "Remove Order Book for a given Asset Pair. Attention! Use this method only when clients can't place orders on this pair!",
    httpMethod = "DELETE"
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path")
    ))
  def orderBookDelete: Route = (path("orderbook" / AssetPairPM) & delete & withAuth) { p =>
    withAssetPair(p) { pair =>
      unavailableOrderBookBarrier(pair) {
        complete(storeEvent(QueueEvent.OrderBookDeleted(pair)).map {
          case None => NotImplemented(MatcherError.FeatureDisabled)
          case _    => SimpleResponse(StatusCodes.Accepted, "Deleting order book")
        })
      }
    }
  }

  @Path("/transactions/{orderId}")
  @ApiOperation(value = "Get Exchange Transactions for order",
                notes = "Get all exchange transactions created by DEX on execution of the given order",
                httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "orderId", value = "Order Id", dataType = "string", paramType = "path")
    ))
  def getTransactionsByOrder: Route = (path("transactions" / ByteStrPM) & get) { orderId =>
    complete(StatusCodes.OK -> Json.toJson(DBUtils.transactionsForOrder(db, orderId)))
  }

  @Path("/debug/currentOffset")
  @ApiOperation(value = "Get a current offset in the queue", notes = "", httpMethod = "GET")
  def getCurrentOffset: Route = (path("debug" / "currentOffset") & get & withAuth) {
    complete(StatusCodes.OK -> currentOffset())
  }

  @Path("/debug/lastOffset")
  @ApiOperation(value = "Get the last offset in the queue", notes = "", httpMethod = "GET")
  def getLastOffset: Route = (path("debug" / "lastOffset") & get & withAuth) {
    complete(lastOffset().map(StatusCodes.OK -> _))
  }

  @Path("/debug/oldestSnapshotOffset")
  @ApiOperation(value = "Get the oldest snapshot's offset in the queue", notes = "", httpMethod = "GET")
  def getOldestSnapshotOffset: Route = (path("debug" / "oldestSnapshotOffset") & get & withAuth) {
    complete {
      (matcher ? GetSnapshotOffsets).mapTo[SnapshotOffsetsResponse].map { response =>
        StatusCodes.OK -> response.offsets.valuesIterator.collect { case Some(x) => x }.min
      }
    }
  }

  @Path("/debug/allSnapshotOffsets")
  @ApiOperation(value = "Get all snapshots' offsets in the queue", notes = "", httpMethod = "GET")
  def getAllSnapshotOffsets: Route = (path("debug" / "allSnapshotOffsets") & get & withAuth) {
    complete {
      (matcher ? GetSnapshotOffsets).mapTo[SnapshotOffsetsResponse].map { x =>
        val js = Json.obj(
          x.offsets.collect {
            case (assetPair, Some(offset)) => assetPair.key -> Json.toJsFieldJsValueWrapper(offset)
          }.toSeq: _*
        )

        StatusCodes.OK -> js
      }
    }
  }
}

object MatcherApiRoute {
  private def stringifyAssetIds(balances: Map[Asset, Long]): Map[String, Long] =
    balances.map { case (aid, v) => AssetPair.assetIdStr(aid) -> v }
}

package com.wavesplatform.matcher.api

import java.util.concurrent.Executors

import akka.actor.ActorRef
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import akka.http.scaladsl.server.{Directive1, Route}
import akka.pattern.ask
import akka.util.Timeout
import com.google.common.primitives.Longs
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.api.http._
import com.wavesplatform.crypto
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.matcher.market.MatcherActor.{GetMarkets, GetMarketsResponse}
import com.wavesplatform.matcher.market.MatcherTransactionWriter.GetTransactionsByOrder
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.market.OrderHistoryActor
import com.wavesplatform.matcher.market.OrderHistoryActor.GetTradableBalance
import com.wavesplatform.matcher.model._
import com.wavesplatform.matcher.{AssetPairBuilder, MatcherSettings}
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.{Blockchain, ByteStr}
import com.wavesplatform.transaction.assets.exchange.OrderJson._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.{Base58, NTP, ScorexLogging}
import com.wavesplatform.wallet.Wallet
import io.swagger.annotations._
import javax.ws.rs.Path
import kamon.Kamon
import org.iq80.leveldb.DB
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.{Failure, Success, Try}

@Path("/matcher")
@Api(value = "/matcher/")
case class MatcherApiRoute(wallet: Wallet,
                           assetPairBuilder: AssetPairBuilder,
                           matcher: ActorRef,
                           orderHistory: ActorRef,
                           orderBook: AssetPair => Option[ActorRef],
                           orderBookSnapshot: OrderBookSnapshotHttpCache,
                           txWriter: ActorRef,
                           settings: RestAPISettings,
                           matcherSettings: MatcherSettings,
                           blockchain: Blockchain,
                           db: DB)
    extends ApiRoute
    with ScorexLogging {

  import MatcherApiRoute._
  import PathMatchers._

  private val timer       = Kamon.timer("matcher.api-requests")
  private val placeTimer  = timer.refine("action" -> "place")
  private val cancelTimer = timer.refine("action" -> "cancel")

  override lazy val route: Route =
    pathPrefix("matcher") {
      matcherPublicKey ~ getOrderBook ~ marketStatus ~ place ~ getAssetPairAndPublicKeyOrderHistory ~ getPublicKeyOrderHistory ~
        getAllOrderHistory ~ getTradableBalance ~ reservedBalance ~ orderStatus ~
        historyDelete ~ cancel ~ orderbooks ~ orderBookDelete ~ getTransactionsByOrder ~ forceCancelOrder ~
        getSettings
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

  @Path("/")
  @ApiOperation(value = "Matcher Public Key", notes = "Get matcher public key", httpMethod = "GET")
  def matcherPublicKey: Route = (pathEndOrSingleSlash & get) {
    complete(
      wallet
        .findPrivateKey(matcherSettings.account)
        .map(a => JsString(Base58.encode(a.publicKey)))
        .getOrElse[JsValue](JsString("")))
  }

  @Path("/settings")
  @ApiOperation(value = "Matcher Settings", notes = "Get matcher settings", httpMethod = "GET")
  def getSettings: Route = (path("settings") & get) {
    val priceAssets = matcherSettings.priceAssets
    complete(StatusCodes.OK -> Json.obj("priceAssets" -> priceAssets))
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
      complete((matcher ? GetMarketStatusRequest(pair)).mapTo[MatcherResponse])
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
    (pathEndOrSingleSlash & post) {
      json[Order] { order =>
        placeTimer.measure {
          if (blockchain.hasScript(order.senderPublicKey.toAddress) &&
              !blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading, blockchain.height)) {
            Future.successful[MatcherResponse](StatusCodes.BadRequest -> "Trading on scripted account isn't allowed yet.")
          } else {
            log.trace(s"Placing ${order.id()}")
            (matcher ? order).mapTo[MatcherResponse]
          }
        }
      }
    }
  }

  private def cancelOrder(orderId: ByteStr, senderPublicKey: Option[PublicKeyAccount]): ToResponseMarshallable = {
    val st = cancelTimer.start()
    DBUtils.orderInfo(db, orderId).status match {
      case LimitOrder.NotFound      => StatusCodes.NotFound
      case status if status.isFinal => StatusCodes.BadRequest -> Json.obj("message" -> s"Order is already ${status.name}")
      case _ =>
        DBUtils.order(db, orderId) match {
          case None =>
            log.warn(s"Order $orderId was not found in history")
            StatusCodes.NotFound
          case Some(order) if senderPublicKey.exists(_ != order.senderPublicKey) =>
            StatusCodes.BadRequest -> Json.obj("message" -> "Public Key mismatch")
          case Some(order) =>
            orderBook(order.assetPair) match {
              case Some(orderBookRef) =>
                log.trace(s"Cancelling ${order.id()} for ${senderPublicKey.map(_.address)}")
                (orderBookRef ? CancelOrder(orderId)).mapTo[MatcherResponse].andThen { case _ => st.stop() }
              case None =>
                log.debug(s"Order book for ${order.assetPair} was not found, cancelling $orderId anyway")
                (orderHistory ? OrderHistoryActor.ForceCancelOrderFromHistory(orderId))
                  .mapTo[Option[Order]]
                  .map {
                    case None    => StatusCodes.NotFound
                    case Some(_) => StatusCodes.OK
                  }
                  .andThen { case _ => st.stop() }
            }
        }
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
    withAssetPair(p) { pair =>
      orderBook(pair).fold[Route](complete(StatusCodes.NotFound -> Json.obj("message" -> "Invalid asset pair"))) { _ =>
        json[CancelOrderRequest] { req =>
          if (req.isSignatureValid()) req.orderId match {
            case Some(id) => cancelOrder(id, Some(req.sender))
            case None     => NotImplemented("Batch cancel is not supported yet")
          } else InvalidSignature
        }
      }
    }
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
  def historyDelete: Route = (path("orderbook" / AssetPairPM / "delete") & post) { p =>
    json[CancelOrderRequest] { req =>
      req.orderId.fold[MatcherResponse](NotImplemented("Batch order deletion is not supported yet"))(OrderDeleted)
    }
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
      new ApiImplicitParam(name = "Timestamp", value = "Timestamp", required = true, dataType = "integer", paramType = "header"),
      new ApiImplicitParam(name = "Signature",
                           value = "Signature of [Public Key ++ Timestamp] bytes",
                           required = true,
                           dataType = "string",
                           paramType = "header")
    ))
  def getAssetPairAndPublicKeyOrderHistory: Route = (path("orderbook" / AssetPairPM / "publicKey" / PublicKeyPM) & get) { (p, publicKey) =>
    (headerValueByName("Timestamp") & headerValueByName("Signature")) { (ts, sig) =>
      checkGetSignature(publicKey, ts, sig) match {
        case Success(address) =>
          withAssetPair(p, redirectToInverse = true, s"/publicKey/$publicKey") { pair =>
            complete(StatusCodes.OK -> DBUtils.ordersByAddressAndPair(db, address, pair, matcherSettings.maxOrdersPerRequest).map {
              case (order, orderInfo) =>
                orderJson(order, orderInfo)
            })
          }
        case Failure(ex) =>
          complete(StatusCodes.BadRequest -> Json.obj("message" -> ex.getMessage))
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
      (headerValueByName("Timestamp") & headerValueByName("Signature")) { (ts, sig) =>
        checkGetSignature(publicKey, ts, sig) match {
          case Success(_) =>
            complete(
              StatusCodes.OK -> DBUtils
                .ordersByAddress(db, publicKey, activeOnly.getOrElse(false), matcherSettings.maxOrdersPerRequest)
                .map {
                  case (order, orderInfo) =>
                    orderJson(order, orderInfo)
                })
          case Failure(ex) =>
            complete(StatusCodes.BadRequest -> Json.obj("message" -> ex.getMessage))
        }
      }
    }
  }

  def checkGetSignature(pk: PublicKeyAccount, timestamp: String, signature: String): Try[PublicKeyAccount] = Try {
    val sig = Base58.decode(signature).get
    val ts  = timestamp.toLong
    require(math.abs(ts - NTP.correctedTime()).millis < matcherSettings.maxTimestampDiff, "Incorrect timestamp")
    require(crypto.verify(sig, pk.publicKey ++ Longs.toByteArray(ts), pk.publicKey), "Incorrect signature")
    pk
  }

  @Path("/orders/cancel/{orderId}")
  @ApiOperation(value = "Cancel Order by ID without signature", notes = "Cancel Order by ID without signature", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "orderId", value = "Order Id", required = true, dataType = "string", paramType = "path")
    ))
  def forceCancelOrder: Route = (path("orders" / "cancel" / ByteStrPM) & post & withAuth) { orderId =>
    complete(cancelOrder(orderId, None))
  }

  @Path("/orders/{address}")
  @ApiOperation(value = "All Order History by address", notes = "Get All Order History for a given address", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", dataType = "string", paramType = "path")
    ))
  def getAllOrderHistory: Route = (path("orders" / AddressPM) & get & withAuth) { address =>
    parameters('activeOnly.as[Boolean].?) { activeOnly =>
      complete(StatusCodes.OK -> DBUtils.ordersByAddress(db, address, activeOnly.getOrElse(true), matcherSettings.maxOrdersPerRequest).map {
        case (order, orderInfo) =>
          orderJson(order, orderInfo)
      })
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
  def getTradableBalance: Route = (path("orderbook" / AssetPairPM / "tradableBalance" / AddressPM) & get) { (pair, address) =>
    withAssetPair(pair, redirectToInverse = true, s"/tradableBalance/$address") { pair =>
      complete((orderHistory ? GetTradableBalance(pair, address, NTP.correctedTime())).mapTo[MatcherResponse])
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
    (headerValueByName("Timestamp") & headerValueByName("Signature")) { (ts, sig) =>
      checkGetSignature(publicKey, ts, sig) match {
        case Success(pk) =>
          complete(StatusCodes.OK -> Json.toJson(DBUtils.reservedBalance(db, pk).map { case (k, v) => AssetPair.assetIdStr(k) -> v }))
        case Failure(ex) =>
          complete(StatusCodes.BadRequest -> Json.obj("message" -> ex.getMessage))
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
      complete(StatusCodes.OK -> DBUtils.orderInfo(db, orderId).status.json)
    }
  }

  @Path("/orderbook")
  @ApiOperation(value = "Get the open trading markets", notes = "Get the open trading markets along with trading pairs meta data", httpMethod = "GET")
  def orderbooks: Route = path("orderbook") {
    (pathEndOrSingleSlash & get) {
      complete((matcher ? GetMarkets).mapTo[GetMarketsResponse])
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}")
  @ApiOperation(value = "Remove Order Book for a given Asset Pair", notes = "Remove Order Book for a given Asset Pair", httpMethod = "DELETE")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path")
    ))
  def orderBookDelete: Route = (path("orderbook" / AssetPairPM) & delete & withAuth) { p =>
    withAssetPair(p) { pair =>
      complete((matcher ? DeleteOrderBookRequest(pair)).mapTo[MatcherResponse])
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
    complete((txWriter ? GetTransactionsByOrder(orderId)).mapTo[MatcherResponse])
  }
}

object MatcherApiRoute {
  private implicit val timeout: Timeout = 5.seconds

  val cancelExecutor: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  val expiration = 15.minutes

  val cancelRequestsTimestamps: scala.collection.mutable.Map[String, Duration] =
    scala.collection.mutable.Map().withDefaultValue(NTP.correctedTime().millis)

  def checkReuse(address: String, timestamp: Duration) = synchronized {
    val old = cancelRequestsTimestamps(address)
    if (old >= timestamp) {
      true
    } else {
      cancelRequestsTimestamps(address) = timestamp
      false
    }
  }

  def checkTimestamp(address: String, timestamp: Duration)(proc: => Future[(StatusCode, JsValue)]): Future[(StatusCode, JsValue)] = {
    val correct = NTP.correctedTime().millis
    val delta   = timestamp - correct
    if (delta < -60.second) {
      Future.successful(StatusCodes.BadRequest -> Json.obj("message" -> "Timestamp is from future"))
    } else if (delta > expiration) {
      Future.successful(StatusCodes.BadRequest -> Json.obj("message" -> "Timestamp is too old"))
    } else if (checkReuse(address, timestamp)) {
      Future.successful(StatusCodes.BadRequest -> Json.obj("message" -> "Timestamp has already been used"))
    } else {
      proc
    }
  }

  def orderJson(order: Order, orderInfo: OrderInfo): JsObject =
    Json.obj(
      "id"        -> order.idStr(),
      "type"      -> order.orderType.toString,
      "amount"    -> order.amount,
      "price"     -> order.price,
      "timestamp" -> order.timestamp,
      "filled"    -> orderInfo.filled,
      "status"    -> orderInfo.status.name,
      "assetPair" -> order.assetPair.json
    )
}

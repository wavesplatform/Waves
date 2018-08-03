package com.wavesplatform.matcher.api

import java.util.concurrent.Executors
import javax.ws.rs.Path

import akka.actor.ActorRef
import akka.http.scaladsl.model.{StatusCode, StatusCodes, Uri}
import akka.http.scaladsl.server.{Directive1, Route}
import akka.pattern.ask
import akka.util.Timeout
import com.google.common.primitives.Longs
import com.wavesplatform.crypto
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.market.MatcherActor.{GetMarkets, GetMarketsResponse}
import com.wavesplatform.matcher.market.MatcherTransactionWriter.GetTransactionsByOrder
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.market.OrderHistoryActor._
import com.wavesplatform.settings.RestAPISettings
import io.swagger.annotations._
import play.api.libs.json._
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.api.http._
import com.wavesplatform.utils.{Base58, NTP}
import com.wavesplatform.transaction.assets.exchange.OrderJson._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.wallet.Wallet

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

object MatcherApiRoute {
  def apply(wallet: Wallet,
            matcher: ActorRef,
            orderHistory: ActorRef,
            txWriter: ActorRef,
            settings: RestAPISettings,
            matcherSettings: MatcherSettings) = new MatcherApiRoute(wallet, matcher, orderHistory, txWriter, settings, matcherSettings)

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
}

@Path("/matcher")
@Api(value = "/matcher/")
class MatcherApiRoute(wallet: Wallet,
                      matcher: ActorRef,
                      orderHistory: ActorRef,
                      txWriter: ActorRef,
                      val settings: RestAPISettings,
                      matcherSettings: MatcherSettings)
    extends ApiRoute {
  private implicit val timeout: Timeout = 5.seconds

  override lazy val route: Route =
    pathPrefix("matcher") {
      matcherPublicKey ~ orderBook ~ marketStatus ~ place ~ getAssetPairAndPublicKeyOrderHistory ~ getPublicKeyOrderHistory ~
        getAllOrderHistory ~ getTradableBalance ~ reservedBalance ~ orderStatus ~
        historyDelete ~ cancel ~ cancelAll ~ orderbooks ~ orderBookDelete ~ getTransactionsByOrder ~ forceCancelOrder ~
        getSettings
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
  def orderBook: Route = (path("orderbook" / Segment / Segment) & get) { (a1, a2) =>
    parameters('depth.as[Int].?) { depth =>
      withAssetPair(a1, a2) { pair =>
        onComplete((matcher ? GetOrderBookRequest(pair, depth)).mapTo[MatcherResponse]) {
          case Success(resp) =>
            resp.code match {
              case StatusCodes.Found => redirect(Uri(s"/matcher/orderbook/$a2/$a1"), StatusCodes.Found)
              case code              => complete(code -> resp.json)
            }
          case Failure(ex) => complete(StatusCodes.InternalServerError -> s"An error occurred: ${ex.getMessage}")
        }
      }
    }
  }

  @Path("/market/{amountAsset}/{priceAsset}")
  @ApiOperation(value = "Get Information for A Market", notes = "Get basic market information", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path")
    ))
  def marketStatus: Route = (path("orderbook" / Segment / Segment / "status") & get) { (a1, a2) =>
    withAssetPair(a1, a2) { pair =>
      onComplete((matcher ? GetMarketStatusRequest(pair)).mapTo[MatcherResponse]) { t =>
        complete(t.fold(ex => StatusCodes.InternalServerError -> s"An error occurred: ${ex.getMessage}", resp => resp.code -> resp.json))
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
    (pathEndOrSingleSlash & post) {
      json[Order] { order =>
        (matcher ? order)
          .mapTo[MatcherResponse]
          .map(r => r.code -> r.json)
      }
    }
  }

  @Path("/orderbook/cancel")
  @ApiOperation(
    value = "Cancel orders",
    notes = "Cancel all previously submitted orders if it's not already filled completely",
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
    implicit val ec = MatcherApiRoute.cancelExecutor
    json[CancelOrderRequest] { req =>
      if (req.isSignatureValid) {
        req.timestamp match {
          case None => InvalidSignature
          case Some(timestamp) =>
            val address = req.senderPublicKey.address
            MatcherApiRoute.checkTimestamp(address, timestamp.millis) {
              (orderHistory ? GetAllOrderHistory(address, activeOnly = true, timestamp, internal = true))
                .mapTo[GetOrderHistoryResponse]
                .flatMap { res: GetOrderHistoryResponse =>
                  Future
                    .sequence(res.history map {
                      case (id, _, Some(order)) =>
                        matcher ? CancelOrder(order.assetPair, req.senderPublicKey, id)
                      case _ => Future.successful(())
                    })
                    .map(_ => StatusCodes.OK -> Json.obj("status" -> "Cancelled"))
                }
            }
        }
      } else {
        InvalidSignature
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
  def cancel: Route = (path("orderbook" / Segment / Segment / "cancel") & post) { (a1, a2) =>
    withAssetPair(a1, a2) { pair =>
      json[CancelOrderRequest] { req =>
        if (req.isSignatureValid) {
          req.orderId match {
            case Some(id) =>
              (matcher ? CancelOrder(pair, req.senderPublicKey, Base58.encode(id)))
                .mapTo[MatcherResponse]
                .map(r => r.code -> r.json)
            case None =>
              implicit val ec = MatcherApiRoute.cancelExecutor
              val timestamp   = req.timestamp.get
              val address     = req.senderPublicKey.address
              MatcherApiRoute.checkTimestamp(address, timestamp.millis) {
                (orderHistory ? GetOrderHistory(pair, address, timestamp, internal = true))
                  .mapTo[GetOrderHistoryResponse]
                  .flatMap { res =>
                    Future
                      .sequence(res.history map { h =>
                        matcher ? CancelOrder(pair, req.senderPublicKey, h._1)
                      })
                      .map(_ => StatusCodes.OK -> Json.obj("status" -> "Cancelled"))
                  }
              }
          }
        } else InvalidSignature
      }
    }
  }
  @Path("/orderbook/{amountAsset}/{priceAsset}/delete")
  @ApiOperation(
    value = "Delete Order from History by Id",
    notes = "Delete Order from History by Id if it's in terminal status (Filled, Cancel)",
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
  def historyDelete: Route = (path("orderbook" / Segment / Segment / "delete") & post) { (a1, a2) =>
    withAssetPair(a1, a2) { pair =>
      json[CancelOrderRequest] { req =>
        if (req.isSignatureValid) {
          (orderHistory ? DeleteOrderFromHistory(pair, req))
            .mapTo[MatcherResponse]
            .map(r => r.code -> r.json)
        } else InvalidSignature
      }
    }
  }

  def withAssetPair(a1: String, a2: String): Directive1[AssetPair] = {
    AssetPair.createAssetPair(a1, a2) match {
      case Success(p) => provide(p)
      case Failure(_) => complete(StatusCodes.BadRequest -> Json.obj("message" -> "Invalid asset pair"))
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
  def getAssetPairAndPublicKeyOrderHistory: Route = (path("orderbook" / Segment / Segment / "publicKey" / Segment) & get) { (a1, a2, publicKey) =>
    (headerValueByName("Timestamp") & headerValueByName("Signature")) { (ts, sig) =>
      checkGetSignature(publicKey, ts, sig) match {
        case Success(address) =>
          withAssetPair(a1, a2) { pair =>
            complete(
              (orderHistory ? GetOrderHistory(pair, address, NTP.correctedTime(), internal = false))
                .mapTo[MatcherResponse]
                .map(r => r.code -> r.json))
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
  def getPublicKeyOrderHistory: Route = (path("orderbook" / Segment) & get) { publicKey =>
    parameters('activeOnly.as[Boolean].?) { activeOnly =>
      (headerValueByName("Timestamp") & headerValueByName("Signature")) { (ts, sig) =>
        checkGetSignature(publicKey, ts, sig) match {
          case Success(address) =>
            complete(
              (orderHistory ? GetAllOrderHistory(address, activeOnly.getOrElse(false), NTP.correctedTime(), internal = false))
                .mapTo[MatcherResponse]
                .map(r => r.code -> r.json))
          case Failure(ex) =>
            complete(StatusCodes.BadRequest -> Json.obj("message" -> ex.getMessage))
        }
      }
    }
  }

  def checkGetSignature(publicKey: String, timestamp: String, signature: String): Try[String] = {
    Try {
      val pk  = Base58.decode(publicKey).get
      val sig = Base58.decode(signature).get
      val ts  = timestamp.toLong
      require(math.abs(ts - NTP.correctedTime()).millis < matcherSettings.maxTimestampDiff, "Incorrect timestamp")
      require(crypto.verify(sig, pk ++ Longs.toByteArray(ts), pk), "Incorrect signature")
      PublicKeyAccount(pk).address
    }
  }

  @Path("/orders/cancel/{orderId}")
  @ApiOperation(value = "Cancel Order by ID without signature", notes = "Cancel Order by ID without signature", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "orderId", value = "Order Id", required = true, dataType = "string", paramType = "path")
    ))
  def forceCancelOrder: Route = (path("orders" / "cancel" / Segment) & post & withAuth) { orderId =>
    implicit val timeout = Timeout(10.seconds)
    val resp: Future[MatcherResponse] = (orderHistory ? ForceCancelOrderFromHistory(orderId)).flatMap {
      case Some(order: Order) => (matcher ? ForceCancelOrder(order.assetPair, orderId)).mapTo[MatcherResponse]
      case None =>
        Future {
          OrderCancelRejected("Order not found")
        }
    }

    complete(resp.map(r => r.code -> r.json))
  }

  @Path("/orders/{address}")
  @ApiOperation(value = "All Order History by address", notes = "Get All Order History for a given address", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", dataType = "string", paramType = "path")
    ))
  def getAllOrderHistory: Route = (path("orders" / Segment) & get & withAuth) { addr =>
    implicit val timeout: Timeout = Timeout(10.seconds)
    complete(
      (orderHistory ? GetAllOrderHistory(addr, activeOnly = true, NTP.correctedTime(), internal = false))
        .mapTo[MatcherResponse]
        .map(r => r.code -> r.json))
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}/tradableBalance/{address}")
  @ApiOperation(value = "Tradable balance for Asset Pair", notes = "Get Tradable balance for the given Asset Pair", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "address", value = "Account Address", required = true, dataType = "string", paramType = "path")
    ))
  def getTradableBalance: Route = (path("orderbook" / Segment / Segment / "tradableBalance" / Segment) & get) { (a1, a2, address) =>
    withAssetPair(a1, a2) { pair =>
      complete(
        (orderHistory ? GetTradableBalance(pair, address, NTP.correctedTime()))
          .mapTo[MatcherResponse]
          .map(r => r.code -> r.json))
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
  def reservedBalance: Route = (path("balance" / "reserved" / Segment) & get) { publicKey =>
    (headerValueByName("Timestamp") & headerValueByName("Signature")) { (ts, sig) =>
      checkGetSignature(publicKey, ts, sig) match {
        case Success(address) =>
          complete(
            (orderHistory ? GetOpenPortfolio(address, NTP.correctedTime()))
              .mapTo[MatcherResponse]
              .map(r => r.code -> r.json))
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
  def orderStatus: Route = (path("orderbook" / Segment / Segment / Segment) & get) { (a1, a2, orderId) =>
    withAssetPair(a1, a2) { pair =>
      complete(
        (orderHistory ? GetOrderStatus(pair, orderId, NTP.correctedTime()))
          .mapTo[MatcherResponse]
          .map(r => r.code -> r.json))
    }
  }

  @Path("/orderbook")
  @ApiOperation(value = "Get the open trading markets", notes = "Get the open trading markets along with trading pairs meta data", httpMethod = "GET")
  def orderbooks: Route = path("orderbook") {
    (pathEndOrSingleSlash & get) {
      complete(
        (matcher ? GetMarkets)
          .mapTo[GetMarketsResponse]
          .map(r => r.json))
    }
  }

  @Path("/orderbook/{amountAsset}/{priceAsset}")
  @ApiOperation(value = "Remove Order Book for a given Asset Pair", notes = "Remove Order Book for a given Asset Pair", httpMethod = "DELETE")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "amountAsset", value = "Amount Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "priceAsset", value = "Price Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path")
    ))
  def orderBookDelete: Route = (path("orderbook" / Segment / Segment) & delete & withAuth) { (a1, a2) =>
    withAssetPair(a1, a2) { pair =>
      complete(
        (matcher ? DeleteOrderBookRequest(pair))
          .mapTo[MatcherResponse]
          .map(r => r.code -> r.json))
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
  def getTransactionsByOrder: Route = (path("transactions" / Segment) & get) { orderId =>
    complete(
      (txWriter ? GetTransactionsByOrder(orderId))
        .mapTo[MatcherResponse]
        .map(r => r.code -> r.json))
  }
}

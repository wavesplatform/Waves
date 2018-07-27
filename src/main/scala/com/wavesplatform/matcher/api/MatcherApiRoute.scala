package com.wavesplatform.matcher.api

import akka.actor.ActorRef
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Directive1, Route}
import akka.pattern.ask
import akka.util.Timeout
import com.google.common.primitives.Longs
import com.wavesplatform.crypto
import com.wavesplatform.matcher.market.MatcherActor.{GetMarkets, GetMarketsResponse}
import com.wavesplatform.matcher.market.MatcherTransactionWriter.GetTransactionsByOrder
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.market.OrderHistoryActor._
import com.wavesplatform.matcher.model.MatcherModel.{Level, Price}
import com.wavesplatform.matcher.model.{LevelAgg, LimitOrder, OrderBook, OrderInfo}
import com.wavesplatform.matcher.{AssetPairBuilder, MatcherSettings}
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.utils.Base58
import io.swagger.annotations._
import javax.ws.rs.Path
import kamon.Kamon
import org.iq80.leveldb.DB
import play.api.libs.json._
import scorex.account.PublicKeyAccount
import scorex.api.http._
import scorex.transaction.assets.exchange.OrderJson._
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scorex.utils.NTP
import scorex.wallet.Wallet

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

@Path("/matcher")
@Api(value = "/matcher/")
case class MatcherApiRoute(wallet: Wallet,
                           assetPairBuilder: AssetPairBuilder,
                           matcher: ActorRef,
                           orderHistory: ActorRef,
                           orderBook: AssetPair => Option[ActorRef],
                           orderBookSnapshot: AssetPair => Option[OrderBook],
                           txWriter: ActorRef,
                           settings: RestAPISettings,
                           matcherSettings: MatcherSettings,
                           db: DB)
    extends ApiRoute {

  import MatcherApiRoute._
  import PathMatchers._

  private val timer       = Kamon.timer("matcher.api-requests")
  private val placeTimer  = timer.refine("action" -> "place")
  private val cancelTimer = timer.refine("action" -> "cancel")

  override lazy val route: Route =
    pathPrefix("matcher") {
      matcherPublicKey ~ getOrderBook ~ place ~ getAssetPairAndPublicKeyOrderHistory ~ getPublicKeyOrderHistory ~
        getAllOrderHistory ~ getTradableBalance ~ reservedBalance ~ orderStatus ~
        historyDelete ~ cancel ~ orderbooks ~ orderBookDelete ~ getTransactionsByOrder ~ forceCancelOrder
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
        complete(StatusCodes.OK -> orderBookSnapshot(pair).fold(GetOrderBookResponse.empty(pair))(handleGetOrderBook(pair, _, depth)).json)
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
        dataType = "scorex.transaction.assets.exchange.Order"
      )
    ))
  def place: Route = path("orderbook") {
    (pathEndOrSingleSlash & post) {
      json[Order] { order =>
        placeTimer.measure {
          (matcher ? order).mapTo[MatcherResponse].map(r => r.code -> r.json)
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
      orderBook(pair).fold[Route](complete(StatusCodes.NotFound -> Json.obj("message" -> "Invalid asset pair"))) { oba =>
        json[CancelOrderRequest] { req =>
          if (req.isSignatureValid()) cancelTimer.measure {
            (oba ? CancelOrder(pair, req))
              .mapTo[MatcherResponse]
              .map(r => r.code -> r.json)
          } else {
            OrderCancelRejected("Invalid signature").json
          }
        }
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
  def historyDelete: Route = (path("orderbook" / AssetPairPM / "delete") & post) { p =>
    withAssetPair(p) { pair =>
      json[CancelOrderRequest] { req =>
        if (req.isSignatureValid()) {
          (orderHistory ? DeleteOrderFromHistory(pair, req.senderPublicKey, req.orderId, NTP.correctedTime()))
            .mapTo[MatcherResponse]
            .map(r => r.code -> r.json)
        } else {
          StatusCodes.BadRequest -> Json.obj("message" -> "Incorrect signature")
        }
      }
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
            DBUtils.ordersByAddress(db, address, Set(pair.priceAsset, pair.amountAsset), activeOnly = true)

            // todo: load order history directly from leveldb
            complete(StatusCodes.NotImplemented)
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
          case Success(address) =>
            complete(StatusCodes.OK -> DBUtils.ordersByAddress(db, address, Set.empty, activeOnly.getOrElse(false)).map {
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
    implicit val timeout: Timeout = Timeout(10.seconds)
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
  def getAllOrderHistory: Route = (path("orders" / AddressPM) & get & withAuth) { _ =>
    // todo: load order history directly from leveldb
    complete(StatusCodes.NotImplemented)
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
      val status = DBUtils.orderInfo(db, orderId).status
      val code   = if (status == LimitOrder.NotFound) StatusCodes.NotFound else StatusCodes.OK
      complete(code -> status.json)
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
  def orderBookDelete: Route = (path("orderbook" / AssetPairPM) & delete & withAuth) { p =>
    withAssetPair(p) { pair =>
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
  def getTransactionsByOrder: Route = (path("transactions" / ByteStrPM) & get) { orderId =>
    complete(
      (txWriter ? GetTransactionsByOrder(orderId))
        .mapTo[MatcherResponse]
        .map(r => r.code -> r.json))
  }
}

object MatcherApiRoute {
  private implicit val timeout: Timeout = 5.seconds

  private def handleGetOrderBook(pair: AssetPair, orderBook: OrderBook, depth: Option[Int]): GetOrderBookResponse = {
    def aggregateLevel(l: (Price, Level[LimitOrder])) = LevelAgg(l._1, l._2.foldLeft(0L)((b, o) => b + o.amount))

    val d = Math.min(depth.getOrElse(MaxDepth), MaxDepth)
    GetOrderBookResponse(pair, orderBook.bids.take(d).map(aggregateLevel).toSeq, orderBook.asks.take(d).map(aggregateLevel).toSeq)
  }

  def orderJson(order: Order, orderInfo: OrderInfo): JsObject =
    Json.obj(
      "id"        -> order.id(),
      "type"      -> order.orderType.toString,
      "amount"    -> order.amount,
      "price"     -> order.price,
      "timestamp" -> order.timestamp,
      "filled"    -> orderInfo.filled,
      "status"    -> orderInfo.status.name,
      "assetPair" -> order.assetPair.json
    )
}

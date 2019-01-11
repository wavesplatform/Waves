package com.wavesplatform.matcher.api

import java.util.concurrent.Executors

import akka.actor.ActorRef
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Directive1, Route}
import akka.pattern.ask
import akka.util.Timeout
import com.google.common.primitives.Longs
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.api.http._
import com.wavesplatform.crypto
import com.wavesplatform.matcher.AssetPairBuilder
import com.wavesplatform.matcher.market.MatcherActor.{GetMarkets, MarketData}
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.market.OrderHistoryActor
import com.wavesplatform.matcher.model._
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.AssetAcc
import com.wavesplatform.transaction.assets.exchange.OrderJson._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.{Base58, ScorexLogging, Time}
import io.netty.util.concurrent.DefaultThreadFactory
import io.swagger.annotations._
import javax.ws.rs.Path
import kamon.Kamon
import org.iq80.leveldb.DB
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

@Path("/matcher")
@Api(value = "/matcher/")
case class MatcherApiRoute(assetPairBuilder: AssetPairBuilder,
                           orderValidator: OrderValidator,
                           matcher: ActorRef,
                           orderHistory: ActorRef,
                           orderBook: AssetPair => Option[Either[Unit, ActorRef]],
                           getMarketStatus: AssetPair => Option[MarketStatus],
                           orderBookSnapshot: OrderBookSnapshotHttpCache,
                           wavesSettings: WavesSettings,
                           db: DB,
                           time: Time)
    extends ApiRoute
    with ScorexLogging {

  import MatcherApiRoute._
  import PathMatchers._
  import wavesSettings._

  override val settings = restAPISettings

  private val timer           = Kamon.timer("matcher.api-requests")
  private val placeTimer      = timer.refine("action" -> "place")
  private val cancelTimer     = timer.refine("action" -> "cancel")
  private val openVolumeTimer = timer.refine("action" -> "open-volume")

  private val batchCancelExecutor = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor(new DefaultThreadFactory("batch-cancel", true)))

  override lazy val route: Route =
    pathPrefix("matcher") {
      matcherPublicKey ~ getOrderBook ~ marketStatus ~ place ~ getAssetPairAndPublicKeyOrderHistory ~ getPublicKeyOrderHistory ~
        getAllOrderHistory ~ getTradableBalance ~ reservedBalance ~ orderStatus ~
        historyDelete ~ cancel ~ cancelAll ~ orderbooks ~ orderBookDelete ~ getTransactionsByOrder ~ forceCancelOrder ~
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
    complete(JsString(Base58.encode(orderValidator.matcherPublicKey.publicKey)))
  }

  @Path("/settings")
  @ApiOperation(value = "Matcher Settings", notes = "Get matcher settings", httpMethod = "GET")
  def getSettings: Route = (path("settings") & get) {
    complete(StatusCodes.OK -> Json.obj("priceAssets" -> matcherSettings.priceAssets))
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
      getMarketStatus(pair).fold(complete(StatusCodes.NotFound -> Json.obj("message" -> "Invalid asset pair"))) { ms =>
        complete(
          StatusCodes.OK -> Json.obj(
            "lastPrice" -> ms.last.map(_.price),
            "lastSide"  -> ms.last.map(_.orderType.toString),
            "bid"       -> ms.bid.map(_._1),
            "bidAmount" -> ms.bid.map(_._2.map(_.amount).sum),
            "ask"       -> ms.ask.map(_._1),
            "askAmount" -> ms.ask.map(_._2.map(_.amount).sum)
          ))
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
        placeTimer.measureFuture {
          orderValidator.validateNewOrder(order) match {
            case Left(e)  => Future.successful[MatcherResponse](OrderRejected(e))
            case Right(_) => (matcher ? order).mapTo[MatcherResponse]
          }
        }
      }
    }
  }

  private def doCancel(order: Order): Future[MatcherResponse] = orderBook(order.assetPair) match {
    case Some(Right(orderBookRef)) =>
      log.trace(s"Canceling ${order.id()} for ${order.sender.address}")
      (orderBookRef ? CancelOrder(order.id())).mapTo[MatcherResponse].map {
        case _: OrderCancelRejected =>
          orderHistory ! Events.OrderCanceled(LimitOrder(order), unmatchable = false)
          OrderCanceled(order.id())
        case x => x
      }
    case Some(Left(_)) => Future.successful(OrderBookUnavailable)
    case None =>
      log.debug(s"Order book for ${order.assetPair} was not found, canceling ${order.id()} anyway")
      (orderHistory ? OrderHistoryActor.ForceCancelOrderFromHistory(order.id()))
        .map {
          case Some(_) => OrderCanceled(order.id())
          case None    => OrderCancelRejected(s"Order ${order.id()} not found")
        }
  }

  private def cancelOrder(orderId: ByteStr, senderPublicKey: Option[PublicKeyAccount], force: Boolean = false): ToResponseMarshallable = {
    val st = cancelTimer.start()
    DBUtils.orderInfo(db, orderId).status match {
      case LimitOrder.NotFound                => StatusCodes.NotFound   -> LimitOrder.NotFound.json
      case status if status.isFinal && !force => StatusCodes.BadRequest -> Json.obj("message" -> s"Order is already ${status.name}")
      case _ =>
        DBUtils.order(db, orderId) match {
          case None =>
            StatusCodes.NotFound -> Json.obj(
              "status"  -> "NotFound",
              "message" -> "The order is not found"
            )
          case Some(order) if senderPublicKey.exists(_ != order.senderPublicKey) =>
            OrderCancelRejected("Public key mismatch")
          case Some(order) =>
            doCancel(order).andThen { case _ => st.stop() }
        }
    }
  }

  private def batchCancel(senderPublicKey: PublicKeyAccount, assetPair: Option[AssetPair], requestTimestamp: Long): Future[ToResponseMarshallable] = {
    (orderHistory ? BatchCancel(senderPublicKey, assetPair, requestTimestamp))
      .mapTo[Either[String, Long]]
      .flatMap {
        case Left(e) => Future.successful[ToResponseMarshallable](OrderCancelRejected(e))
        case Right(_) =>
          val ordersToCancel = assetPair match {
            case Some(p) => DBUtils.ordersByAddressAndPair(db, senderPublicKey, p, true, DBUtils.indexes.active.MaxElements)
            case None    => DBUtils.ordersByAddress(db, senderPublicKey, true, DBUtils.indexes.active.MaxElements)
          }

          ordersToCancel
            .foldLeft(Future.successful(Json.arr())) {
              case (f, (order, _)) =>
                f.zipWith(doCancel(order)) { (r, mr) =>
                  r :+ mr.json
                }(batchCancelExecutor)
            }
            .map(arr => StatusCodes.OK -> Json.obj("status" -> "BatchCancelCompleted", "message" -> arr))
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
          if (req.isSignatureValid()) (req.orderId, req.timestamp) match {
            case (Some(id), None) => cancelOrder(id, Some(req.sender))
            case (None, Some(reqTimestamp)) =>
              batchCancel(req.sender, Some(pair), reqTimestamp).andThen {
                case Failure(exception) => log.debug(s"Error validating batch cancel request from ${req.sender.toAddress} for $pair", exception)
              }
            case _ => StatusCodes.BadRequest -> "Either timestamp or orderId must be provided"
          } else InvalidSignature
        }
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
    json[CancelOrderRequest] { req =>
      if (req.isSignatureValid())
        req.timestamp.fold[Future[ToResponseMarshallable]](Future.successful(OrderCancelRejected("Timestamp must be specified"))) { reqTimestamp =>
          batchCancel(req.sender, None, reqTimestamp)
        } else InvalidSignature
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
  def historyDelete: Route = (path("orderbook" / AssetPairPM / "delete") & post) { _ =>
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
    parameters('activeOnly.as[Boolean].?) { activeOnly =>
      (headerValueByName("Timestamp") & headerValueByName("Signature")) { (ts, sig) =>
        checkGetSignature(publicKey, ts, sig) match {
          case Success(address) =>
            withAssetPair(p, redirectToInverse = true, s"/publicKey/$publicKey") { pair =>
              complete(
                StatusCodes.OK -> DBUtils
                  .ordersByAddressAndPair(db, address, pair, activeOnly.getOrElse(false), matcherSettings.maxOrdersPerRequest)
                  .map {
                    case (order, orderInfo) =>
                      orderJson(order, orderInfo)
                  })
            }
          case Failure(ex) =>
            complete(StatusCodes.BadRequest -> Json.obj("message" -> ex.getMessage))
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
    require(math.abs(ts - time.correctedTime()).millis < matcherSettings.maxTimestampDiff, "Incorrect timestamp")
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
    complete(cancelOrder(orderId, None, force = true))
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
      complete(
        StatusCodes.OK -> Json.obj(
          pair.amountAssetStr -> orderValidator.tradableBalance(AssetAcc(address, pair.amountAsset)),
          pair.priceAssetStr  -> orderValidator.tradableBalance(AssetAcc(address, pair.priceAsset))
        ))
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
          complete(StatusCodes.OK -> Json.toJson(openVolumeTimer.measure(DBUtils.reservedBalance(db, pk).map {
            case (k, v) => AssetPair.assetIdStr(k) -> v
          })))
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
      complete((matcher ? GetMarkets).mapTo[Seq[MarketData]].map { markets =>
        StatusCodes.OK -> Json.obj(
          "matcherPublicKey" -> Base58.encode(orderValidator.matcherPublicKey.publicKey),
          "markets" -> JsArray(markets.map(m =>
            Json.obj(
              "amountAsset"     -> m.pair.amountAssetStr,
              "amountAssetName" -> m.amountAssetName,
              "amountAssetInfo" -> m.amountAssetInfo,
              "priceAsset"      -> m.pair.priceAssetStr,
              "priceAssetName"  -> m.priceAssetName,
              "priceAssetInfo"  -> m.priceAssetinfo,
              "created"         -> m.created
          )))
        )
      })
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
      complete((matcher ? DeleteOrderBookRequest(pair)).map { _ =>
        GetOrderBookResponse(time.correctedTime(), pair, Seq(), Seq()).toHttpResponse
      })
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
}

object MatcherApiRoute {
  private implicit val timeout: Timeout = 5.seconds

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

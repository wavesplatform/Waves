package com.wavesplatform.matcher.api

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.settings.WavesSettings
import io.swagger.annotations._
import play.api.libs.json._
import scorex.api.http._
import scorex.app.Application
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.OrderJson._
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.wallet.Wallet

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

@Path("/matcher")
@Api(value = "/matcher/")
case class MatcherApiRoute(application: Application, matcher: ActorRef)(implicit val settings: WavesSettings,
                                              implicit val context: ActorRefFactory) extends ApiRoute {

  val wallet: Wallet = application.wallet
  val storedState: StoredState = application.blockStorage.state.asInstanceOf[StoredState]

  def postJsonRouteAsync(fn: Future[JsonResponse]): Route = {
    onSuccess(fn) {res: JsonResponse =>
      complete(res.code -> HttpEntity(ContentTypes.`application/json`, res.response.toString))
    }
  }

  override lazy val route =
    pathPrefix("matcher") {
      place ~ matcherPubKey ~ signOrder ~ orderStatus ~ balance ~ orderBook ~ cancel
    }

  @Path("/publicKey")
  @ApiOperation(value = "Matcher Public Key", notes = "Get matcher public key", httpMethod = "GET")
  def matcherPubKey: Route = {
    path("publicKey") {
      getJsonRoute {
        val json = wallet.privateKeyAccount(settings.matcherAccount).map(a => JsString(Base58.encode(a.publicKey))).
          getOrElse(JsString(""))
        JsonResponse(json, StatusCodes.OK)
      }
    }
  }

  @Path("/orders/status/{id}")
  @ApiOperation(value = "Order Status", notes = "Get Order status for a given Asset Pair during the last 30 days", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "id", value = "Order Id", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "asset1", value = "Asset Id", required = true, dataType = "string", paramType = "query"),
    new ApiImplicitParam(name = "asset2", value = "Asset Id or empty for WAVES", required = false, dataType = "string", paramType = "query")
  ))
  def orderStatus: Route = {
    pathPrefix("orders" / "status" / Segment ) { id =>
      parameters('asset1, 'asset2.?) { (asset1, asset2) =>
        val pair = AssetPair(Base58.decode(asset1).toOption, asset2.flatMap(Base58.decode(_).toOption))
        getJsonRoute {
          (matcher ? GetOrderStatus(pair, id))
            .mapTo[OrderBookResponse]
            .map(r => JsonResponse(r.json, r.code))
        }
      }
    }
  }

  @Path("/publicKey/{address}")
  @ApiOperation(value = "Public Key", notes = "Account Public Key", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  def balance: Route = {
    path("publicKey" / Segment) { address =>
      getJsonRoute {
        val json = wallet.privateKeyAccount(address).map(a => JsString(Base58.encode(a.publicKey))).
          getOrElse(JsString(""))
        JsonResponse(json, StatusCodes.OK)
      }
    }
  }

  @Path("/orderBook")
  @ApiOperation(value = "Get Order Book for a given Asset Pair",
    notes = "Get Order Book for a given Asset Pair", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "asset1", value = "Asset Id", required = true, dataType = "string", paramType = "query"),
    new ApiImplicitParam(name = "asset2", value = "Asset Id or empty for WAVES", required = false, dataType = "string", paramType = "query")
  ))
  def orderBook: Route = {
    path("orderBook") {
      parameters('asset1, 'asset2.?, "depth".as[Int].?) { (asset1, asset2, depth) =>
      getJsonRoute {
          val pair = AssetPair(Base58.decode(asset1).toOption, asset2.flatMap(Base58.decode(_).toOption))

          (matcher ? GetOrderBookRequest(pair, depth))
            .mapTo[OrderBookResponse]
            .map(r => JsonResponse(r.json, r.code))
        }
      }
    }
  }

  @Path("/sign")
  @ApiOperation(value = "Sign Order",
    notes = "Create order signed by address from wallet",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Order Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.assets.exchange.Order"
    )
  ))
  def signOrder: Route = path("sign") {
    entity(as[String]) { body =>
      postJsonRouteAsync {
        Try(Json.parse(body)).map { js =>
          js.validate[Order] match {
            case err: JsError =>
              Future.successful(WrongTransactionJson(err).response)
            case JsSuccess(order: Order, _) =>
              Future {
                wallet.privateKeyAccount(order.sender.address).map { sender =>
                  val signed = Order.sign(order, sender)
                  JsonResponse(signed.json, StatusCodes.OK)
                }.getOrElse(InvalidAddress.response)
              }
          }
        }.recover {
          case t => println(t)
            Future.successful(WrongJson.response)
        }.get
      }
    }
  }

  val smallRoute: Route =
    pathPrefix("matcher") {
      get {
        pathSingleSlash {
          complete {
            "Captain on the bridge!"
          }
        } ~
          path("ping") {
            complete("PONG!")
          }
      }
    }

  @Path("/orders/place")
  @ApiOperation(value = "Place order",
    notes = "Place a new limit order (buy or sell)",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.assets.exchange.Order"
    )
  ))
  def place: Route = path("orders" / "place") {
    entity(as[String]) { body =>
      postJsonRouteAsync {
        Try(Json.parse(body)).map { js =>
          js.validate[Order] match {
            case err: JsError =>
              Future.successful(WrongTransactionJson(err).response)
            case JsSuccess(order: Order, _) =>
              (matcher ? order)
                .mapTo[OrderBookResponse]
                .map(r => JsonResponse(r.json, r.code))
          }
        }.recover {
          case t => println(t)
            Future.successful(WrongJson.response)
        }.get
      }
    }
  }

  @Path("/orders/cancel")
  @ApiOperation(value = "Cancel order",
    notes = "Cancel previously submitted order if it's not already filled completely",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "asset1", value = "Asset Id", required = true, dataType = "string", paramType = "query"),
    new ApiImplicitParam(name = "asset2", value = "Asset Id or empty for WAVES", required = false, dataType = "string", paramType = "query"),
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.assets.exchange.OrderCancelTransaction"
    )
  ))
  def cancel: Route = path("orders" / "cancel") {
    parameters('asset1, 'asset2.?) { (asset1, asset2) =>
      entity(as[String]) { body =>
        postJsonRouteAsync {
          Try(Json.parse(body)).map { js =>
            js.validate[CancelOrderRequest] match {
              case err: JsError =>
                Future.successful(WrongTransactionJson(err).response)
              case JsSuccess(req: CancelOrderRequest, _) =>
                val pair = AssetPair(Base58.decode(asset1).toOption, asset2.flatMap(Base58.decode(_).toOption))
                (matcher ? CancelOrder(pair, req))
                  .mapTo[OrderBookResponse]
                  .map(r => JsonResponse(r.json, r.code))
            }
          }.recover {
            case t => println(t)
              Future.successful(WrongJson.response)
          }.get
        }
      }
    }
  }

}
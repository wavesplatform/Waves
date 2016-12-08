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
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderCancelTransaction}
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
      place ~ matcherPubKey ~ signOrder ~ orderStatusWaves ~ balance ~ orderBook ~ orderBookWaves ~ cancel
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

  @Path("/orders/{asset1}/status/{id}")
  @ApiOperation(value = "Order Status", notes = "Get Order status for a given AssetId and WAVES during the last 30 days", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "asset1", value = "AssetId", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "id", value = "Order Id", required = true, dataType = "string", paramType = "path")
  ))
  def orderStatusWaves: Route = {
    pathPrefix("orders" / Segment ) { assetId =>
      val asset = Base58.decode(assetId).toOption
      path("status" / Segment) { id =>
        getJsonRoute {
          (matcher ? GetOrderStatus(AssetPair(None, asset), id))
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

  @Path("/orderBook/{asset1}/{asset2}")
  @ApiOperation(value = "Get Order Book for Asset Pair",
    notes = "Get Order Book for 2 given AssetIds (Not WAVES)", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "asset1", value = "AssetId", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "asset2", value = "AssetId", required = true, dataType = "string", paramType = "path")
  ))
  def orderBook: Route = {
    path("orderBook" / Segment / Segment) { (a1, a2) =>
      getJsonRoute {
        val asset1 = Base58.decode(a1).toOption
        val asset2 = Base58.decode(a2).toOption

        (matcher ? GetOrderBookRequest(AssetPair(asset1, asset2)))
          .mapTo[OrderBookResponse]
          .map(r => JsonResponse(r.json, r.code))
      }
    }
  }

  @Path("/orderBook/{asset1}")
  @ApiOperation(value = "Get Order Book for AssetId and WAVES",
    notes = "Get Order Book for a given AssetId and WAVES", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "asset1", value = "AssetId", required = true, dataType = "string", paramType = "path")
  ))
  def orderBookWaves: Route = {
    path("orderBook" / Segment) { (a) =>
      getJsonRoute {
        val asset = Base58.decode(a).toOption
        val pair = AssetPair(None, asset)

        (matcher ? GetOrderBookRequest(pair))
          .mapTo[OrderBookResponse]
          .map(r => JsonResponse(r.json, r.code))
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
              /*if (validation.validate()) {
                (matcher ? order)
                .mapTo[OrderResponse]
                .map { resp =>
                  JsonResponse(resp.json, StatusCodes.OK)
                }
              } else {
                Future.successful(ValidationErrorJson(validation.errors).response)
              }*/
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
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.assets.exchange.OrderCancelTransaction"
    )
  ))
  def cancel: Route = path("orders" / "cancel") {
    entity(as[String]) { body =>
      postJsonRouteAsync {
        Try(Json.parse(body)).map { js =>
          js.validate[OrderCancelTransaction] match {
            case err: JsError =>
              Future.successful(WrongTransactionJson(err).response)
            case JsSuccess(tx: OrderCancelTransaction, _) =>
              (matcher ? CancelOrder(tx.assetPair, tx))
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
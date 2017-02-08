package com.wavesplatform.matcher.api

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import com.wavesplatform.matcher.market.MatcherActor.{GetMarkets, GetMarketsResponse}
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

  override lazy val route: Route =
    pathPrefix("matcher") {
      matcherPublicKey ~ orderBook ~  place ~ orderStatus ~ cancel ~ orderbooks
    }

  private def getInvalidPairResponse: JsonResponse = {
    JsonResponse(StatusCodeMatcherResponse(StatusCodes.NotFound, "Invalid Asset Pair").json, StatusCodes.BadRequest)
  }

  @Path("/")
  @ApiOperation(value = "Matcher Public Key", notes = "Get matcher public key", httpMethod = "GET")
  def matcherPublicKey: Route =
    pathEndOrSingleSlash {
      getJsonRoute {
        val json = wallet.privateKeyAccount(settings.matcherAccount).map(a => JsString(Base58.encode(a.publicKey))).
          getOrElse(JsString(""))
        JsonResponse(json, StatusCodes.OK)
      }
    }

  @Path("/orderbook/{asset1}/{asset2}")
  @ApiOperation(value = "Get Order Book for a given Asset Pair",
    notes = "Get Order Book for a given Asset Pair", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "asset1", value = "First Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "asset2", value = "Second Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "depth", value = "Limit the number of bid/ask records returned", required = false, dataType = "integer", paramType = "query")
  ))
  def orderBook: Route =
    path("orderbook" / Segment / Segment) { (a1, a2) =>
      get {
        jsonRouteAsync {
          AssetPair.createAssetPair(a1, a2).map { pair =>
            (matcher ? GetOrderBookRequest(pair, None))
              .mapTo[MatcherResponse]
              .map(r => JsonResponse(r.json, r.code))
          }.getOrElse(Future.successful(getInvalidPairResponse))
        }
      }
    }

  @Path("/orderbook")
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
  def place: Route =
    path("orderbook") {
      pathEndOrSingleSlash {
        post {
          entity(as[String]) { body =>
            jsonRouteAsync {
              Try {
                val js = Json.parse(body)
                js.validate[Order] match {
                  case err: JsError =>
                    Future.successful(WrongTransactionJson(err).response)
                  case JsSuccess(order: Order, _) =>
                    (matcher ? order)
                      .mapTo[MatcherResponse]
                      .map(r => JsonResponse(r.json, r.code))
                }
              }.getOrElse(Future.successful(WrongJson.response))
            }
          }
        }
      }
    }

  @Path("/orderbook/{asset1}/{asset2}/cancel")
  @ApiOperation(value = "Cancel order",
    notes = "Cancel previously submitted order if it's not already filled completely",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "asset1", value = "First Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "asset2", value = "Second Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "com.wavesplatform.matcher.api.CancelOrderRequest"
    )
  ))
  def cancel: Route =
    path("orderbook" / Segment / Segment / "cancel") { (a1, a2) =>
      post {
        entity(as[String]) { body =>
          jsonRouteAsync {
            AssetPair.createAssetPair(a1, a2).map { pair =>
              Try {
                val js = Json.parse(body)
                js.validate[CancelOrderRequest] match {
                  case err: JsError =>
                    Future.successful(WrongTransactionJson(err).response)
                  case JsSuccess(req: CancelOrderRequest, _) =>
                    (matcher ? CancelOrder(pair, req))
                      .mapTo[MatcherResponse]
                      .map(r => JsonResponse(r.json, r.code))
                }
              }.getOrElse(Future.successful(WrongJson.response))
            }.getOrElse(Future.successful(getInvalidPairResponse))
          }
        }
      }
    }

  @Path("/orderbook/{asset1}/{asset2}/{orderId}")
  @ApiOperation(value = "Order Status",
    notes = "Get Order status for a given Asset Pair during the last 30 days",
    httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "asset1", value = "First Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "asset2", value = "Second Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "orderId", value = "Order Id", required = true, dataType = "string", paramType = "path")
  ))
  def orderStatus: Route =
    path("orderbook" / Segment / Segment / Segment) { (a1, a2, orderId) =>
      get {
        jsonRouteAsync {
          AssetPair.createAssetPair(a1, a2).map { pair =>
            (matcher ? GetOrderStatus(pair, orderId))
              .mapTo[MatcherResponse]
              .map(r => JsonResponse(r.json, r.code))
          }.getOrElse(Future.successful(getInvalidPairResponse))
        }
      }
    }

  @Path("/orderbook")
  @ApiOperation(value = "Get the open trading markets",
    notes = "Get the open trading markets along with trading pairs meta data", httpMethod = "GET")
  def orderbooks: Route =
    path("orderbook") {
      pathEndOrSingleSlash {
        getJsonRoute {
          (matcher ? GetMarkets())
            .mapTo[GetMarketsResponse]
            .map(r => JsonResponse(r.json, StatusCodes.OK))
        }
      }
    }

}

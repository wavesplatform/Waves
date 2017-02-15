package com.wavesplatform.matcher.api

import javax.ws.rs.Path
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import akka.actor.ActorRef
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Directive1, Route}
import akka.pattern.ask
import akka.util.Timeout
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.market.MatcherActor.{GetMarkets, GetMarketsResponse}
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.settings.RestAPISettings
import io.swagger.annotations._
import play.api.libs.json._
import scorex.api.http._
import scorex.app.Application
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.OrderJson._
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.wallet.Wallet

@Path("/matcher")
@Api(value = "/matcher/")
case class MatcherApiRoute(application: Application, matcher: ActorRef, settings: RestAPISettings, matcherSettings: MatcherSettings) extends ApiRoute {
  private implicit val timeout: Timeout = 5.seconds

  val wallet: Wallet = application.wallet
  val storedState: StoredState = application.blockStorage.state.asInstanceOf[StoredState]

  override lazy val route: Route =
    pathPrefix("matcher") {
      matcherPublicKey ~ orderBook ~ place ~ orderStatus ~ cancel ~ orderbooks
    }

  def withAssetPair(a1: String, a2: String): Directive1[AssetPair] = {
    AssetPair.createAssetPair(a1, a2) match {
      case Success(p) => provide(p)
      case Failure(e) => complete(StatusCodes.BadRequest -> Json.obj("message" -> "Invalid asset pair"))
    }
  }

  @Path("/")
  @ApiOperation(value = "Matcher Public Key", notes = "Get matcher public key", httpMethod = "GET")
  def matcherPublicKey: Route = (pathEndOrSingleSlash & get) {
    complete(wallet.privateKeyAccount(matcherSettings.account)
      .map(a => JsString(Base58.encode(a.publicKey)))
      .getOrElse[JsValue](JsString("")))
  }

  @Path("/orderbook/{asset1}/{asset2}")
  @ApiOperation(value = "Get Order Book for a given Asset Pair",
    notes = "Get Order Book for a given Asset Pair", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "asset1", value = "First Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "asset2", value = "Second Asset Id in Pair, or 'WAVES'", dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "depth", value = "Limit the number of bid/ask records returned", required = false, dataType = "integer", paramType = "query")
  ))
  def orderBook: Route = (path("orderbook" / Segment / Segment) & get) { (a1, a2) =>
    withAssetPair(a1, a2) { pair =>
      complete((matcher ? GetOrderBookRequest(pair, None))
        .mapTo[MatcherResponse]
        .map(r => r.code -> r.json))
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
  def place: Route = path("orderbook") {
    (pathEndOrSingleSlash & post) {
      json[Order] { order =>
        (matcher ? order)
          .mapTo[MatcherResponse]
          .map(r => r.code -> r.json)
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
  def cancel: Route = (path("orderbook" / Segment / Segment / "cancel") & post) { (a1, a2) =>
    withAssetPair(a1, a2) { pair =>
      json[CancelOrderRequest] { req =>
        (matcher ? CancelOrder(pair, req))
          .mapTo[MatcherResponse]
          .map(r => r.code -> r.json)
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
  def orderStatus: Route = (path("orderbook" / Segment / Segment / Segment) & get) { (a1, a2, orderId) =>
    withAssetPair(a1, a2) { pair =>
      complete((matcher ? GetOrderStatus(pair, orderId))
        .mapTo[MatcherResponse]
        .map(r => r.code -> r.json))
    }
  }

  @Path("/orderbook")
  @ApiOperation(value = "Get the open trading markets",
    notes = "Get the open trading markets along with trading pairs meta data", httpMethod = "GET")
  def orderbooks: Route = path("orderbook") {
    (pathEndOrSingleSlash & get) {
      complete((matcher ? GetMarkets)
        .mapTo[GetMarketsResponse]
        .map(r => r.json))
    }
  }
}

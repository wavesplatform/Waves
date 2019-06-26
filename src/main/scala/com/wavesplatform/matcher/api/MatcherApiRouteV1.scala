package com.wavesplatform.matcher.api

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Directive0, Directive1, Route}
import com.wavesplatform.api.http._
import com.wavesplatform.matcher.model._
import com.wavesplatform.matcher.{AssetPairBuilder, Matcher}
import com.wavesplatform.settings.{RestAPISettings, WavesSettings}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.utils.ScorexLogging
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json._

@Path("/api/v1")
@Api(value = "/api v1/")
case class MatcherApiRouteV1(assetPairBuilder: AssetPairBuilder,
                             orderBookSnapshot: OrderBookSnapshotHttpCache,
                             wavesSettings: WavesSettings,
                             matcherStatus: () => Matcher.Status)
    extends ApiRoute
    with ScorexLogging {

  import PathMatchers._
  import wavesSettings._

  override val settings: RestAPISettings = restAPISettings

  override lazy val route: Route = pathPrefix("api" / "v1") {
    matcherStatusBarrier {
      getOrderBook
    }
  }

  private def matcherStatusBarrier: Directive0 = matcherStatus() match {
    case Matcher.Status.Working  => pass
    case Matcher.Status.Starting => complete(DuringStart)
    case Matcher.Status.Stopping => complete(DuringShutdown)
  }

  private def defaultFormatError(e: String): ToResponseMarshallable = StatusCodes.NotFound -> Json.obj("message" -> e)

  private def withAssetPair(p: AssetPair,
                            redirectToInverse: Boolean,
                            suffix: String = "",
                            formatError: String => ToResponseMarshallable = defaultFormatError): Directive1[AssetPair] =
    assetPairBuilder.validateAssetPair(p) match {
      case Right(_) => provide(p)
      case Left(e) if redirectToInverse =>
        assetPairBuilder
          .validateAssetPair(p.reverse)
          .fold(
            _ => complete(formatError(e)),
            _ => redirect(s"/matcher/orderbook/${p.priceAssetStr}/${p.amountAssetStr}$suffix", StatusCodes.MovedPermanently)
          )
      case Left(e) => complete(formatError(e))
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
    )
  )
  def getOrderBook: Route = (path("orderbook" / AssetPairPM) & get) { p =>
    parameters('depth.as[Int].?) { depth =>
      withAssetPair(p, redirectToInverse = true) { pair =>
        complete(orderBookSnapshot.get(pair, depth, MatcherModel.Denormalized))
      }
    }
  }
}

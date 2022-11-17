package com.wavesplatform.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http.*
import com.wavesplatform.blockchain.Processor
import com.wavesplatform.settings.RestAPISettings
import monix.execution.Scheduler
import play.api.libs.json.*

class EvaluateApiRoute(settings: RestAPISettings, override val limitedScheduler: Scheduler, processor: Processor)
    extends ApiRoute
    with TimeLimitedRoute {
  override val route: Route = pathPrefix("utils") { evaluate }

  // TODO support traces flag for auto tests
  def evaluate: Route =
    (path("script" / "evaluate" / AddrSegment) & jsonPostD[JsObject]) { case (address, request) =>
      complete {
        processor
          .getLastResultOrRun(address, request)
          .map(_ ++ request ++ Json.obj("address" -> address.toString))
          .runToFuture(limitedScheduler)
      }
    }

}

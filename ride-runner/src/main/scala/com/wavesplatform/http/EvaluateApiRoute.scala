package com.wavesplatform.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http.*
import com.wavesplatform.blockchain.Processor
import monix.execution.Scheduler
import play.api.libs.json.*

case class EvaluateApiRoute(override val limitedScheduler: Scheduler, processor: Processor)
    extends ApiRoute
    with TimeLimitedRoute {
  override val route: Route = pathPrefix("utils") { evaluate }

  def evaluate: Route =
    (path("script" / "evaluate" / AddrSegment) & jsonPostD[JsObject] & parameter("trace".as[Boolean] ? false)) { case (address, request, trace) =>
      complete {
        processor
          .getLastResultOrRun(address, request)
          .map { orig =>
            val withoutTraces = if (trace) orig else orig - "vars"
            withoutTraces ++ request ++ Json.obj("address" -> address.toString)
          }
          .runToFuture(limitedScheduler)
      }
    }

}

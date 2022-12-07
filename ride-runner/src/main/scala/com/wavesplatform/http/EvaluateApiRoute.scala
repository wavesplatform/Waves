package com.wavesplatform.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.account.Address
import com.wavesplatform.api.http.*
import monix.eval.Task
import play.api.libs.json.*

case class EvaluateApiRoute(routeTimeout: RouteTimeout, evaluateExpr: (Address, JsObject) => Task[JsObject]) extends ApiRoute {
  override val route: Route = pathPrefix("utils") { evaluateEndpoint }

  def evaluateEndpoint: Route =
    (path("script" / "evaluate" / AddrSegment) & jsonPostD[JsObject] & parameter("trace".as[Boolean] ? false)) { case (address, request, trace) =>
      routeTimeout.executeToFuture {
        evaluateExpr(address, request)
          .map { orig =>
            val withoutTraces = if (trace) orig else orig - "vars"
            withoutTraces ++ request ++ Json.obj("address" -> address.toString)
          }
      }
    }
}

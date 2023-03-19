package com.wavesplatform.ride.runner.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http.*
import com.wavesplatform.ride.runner.storage.ScriptRequest
import play.api.libs.json.*

import scala.concurrent.Future

case class EvaluateApiRoute(evaluateExpr: ScriptRequest => Future[JsObject]) extends ApiRoute {
  override val route: Route =
    (path("utils" / "script" / "evaluate" / AddrSegment) & jsonPostD[JsObject] & parameter("trace".as[Boolean] ? false)) {
      case (address, request, trace) =>
        extractExecutionContext { implicit ctx =>
          complete {
            evaluateExpr(ScriptRequest(address, request)).map { orig =>
              val withoutTraces = if (trace) orig else orig - "vars"
              withoutTraces ++ request ++ Json.obj("address" -> address.toString)
            }
          }
        }
    }
}

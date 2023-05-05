package com.wavesplatform.ride.runner.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http.*
import com.wavesplatform.ride.runner.requests.RideScriptRunResult
import com.wavesplatform.ride.runner.storage.RideScriptRunRequest
import play.api.libs.json.*

import scala.concurrent.Future

case class EvaluateApiRoute(evaluateExpr: RideScriptRunRequest => Future[RideScriptRunResult]) extends ApiRoute {
  override val route: Route =
    (path("utils" / "script" / "evaluate" / AddrSegment) & jsonPostD[JsObject] & parameter("trace".as[Boolean] ? false)) {
      case (address, jsRequest, trace) =>
        extractExecutionContext { implicit ctx =>
          complete {
            evaluateExpr(RideScriptRunRequest(address, jsRequest)).map { runResult =>
              val withoutTraces = if (trace) runResult.lastResult else runResult.lastResult - "vars"
              val jsBody        = withoutTraces ++ jsRequest ++ Json.obj("address" -> address.toString)
              (runResult.lastStatus, jsBody)
            }
          }
        }
    }
}

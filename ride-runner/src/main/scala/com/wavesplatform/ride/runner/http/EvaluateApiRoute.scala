package com.wavesplatform.ride.runner.http

import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http.*
import com.wavesplatform.ride.runner.requests.{RideScriptRunRequest, RideScriptRunResult}
import play.api.libs.json.*

import scala.concurrent.Future

case class EvaluateApiRoute(evaluateExpr: RideScriptRunRequest => Future[RideScriptRunResult]) extends ApiRoute {
  override val route: Route =
    (path("utils" / "script" / "evaluate" / AddrSegment)
      & jsonPostD[JsObject]
      & parameter("trace".as[Boolean] ? false)
      & optionalHeaderValueByType(Accept)) { case (address, jsRequest, trace, accept) =>
      extractExecutionContext { implicit ctx =>
        complete {
          val request = RideScriptRunRequest(
            address,
            jsRequest,
            trace = trace,
            intAsString = accept.exists(_.mediaRanges.exists(CustomJson.acceptsNumbersAsStrings))
          )

          evaluateExpr(request).map { runResult =>
            HttpResponse(
              status = runResult.lastStatus,
              entity = HttpEntity(ContentTypes.`application/json`, runResult.lastResult)
            )
          }
        }
      }
    }
}

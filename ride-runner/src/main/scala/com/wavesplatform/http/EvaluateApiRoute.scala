package com.wavesplatform.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http.*
import com.wavesplatform.storage.RequestsStorage.RequestKey
import play.api.libs.json.*

import scala.concurrent.Future

case class EvaluateApiRoute(evaluateExpr: RequestKey => Future[JsObject]) extends ApiRoute {
  override val route: Route = pathPrefix("utils") { evaluateEndpoint }

  def evaluateEndpoint: Route =
    (path("script" / "evaluate" / AddrSegment) & jsonPostD[JsObject] & parameter("trace".as[Boolean] ? false)) { case (address, request, trace) =>
      extractExecutionContext { implicit ctx =>
        complete {
          evaluateExpr((address, request)).map { orig =>
            val withoutTraces = if (trace) orig else orig - "vars"
            withoutTraces ++ request ++ Json.obj("address" -> address.toString)
          }
        }
      }
    }
}

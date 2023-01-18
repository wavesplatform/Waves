package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http.*
import com.wavesplatform.blockchain.RestApiScript
import com.wavesplatform.ride.app.ServiceStatus
import com.wavesplatform.storage.RequestsStorage.RequestKey
import play.api.libs.json.*

import scala.concurrent.Future

case class EvaluateApiRoute(evaluateExpr: RequestKey => Future[JsObject], getServiceStatus: () => ServiceStatus) extends ApiRoute {
  override val route: Route = evaluateEndpoint ~ healthStatus

  def evaluateEndpoint: Route =
    (path("utils" / "script" / "evaluate" / AddrSegment) & jsonPostD[JsObject] & parameter("trace".as[Boolean] ? false)) {
      case (address, request, trace) =>
        extractExecutionContext { implicit ctx =>
          complete {
            evaluateExpr((address, request)).map { orig =>
              val withoutTraces = if (trace) orig else orig - "vars"
              (withoutTraces - RestApiScript.LastUpdatedKey) ++ request ++ Json.obj("address" -> address.toString)
            }
          }
        }
    }

  def healthStatus: Route = path("ride" / "status") {
    val s = getServiceStatus()
    complete(
      if (s.healthy) StatusCodes.OK else StatusCodes.InternalServerError,
      s
    )
  }
}

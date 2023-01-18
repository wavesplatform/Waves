package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http.*
import com.wavesplatform.ride.app.ServiceStatus

case class ServiceStatusRoute(getServiceStatus: () => ServiceStatus) extends ApiRoute {
  override val route: Route = path("ride" / "status") {
    val s = getServiceStatus()
    complete(
      if (s.healthy) StatusCodes.OK else StatusCodes.InternalServerError,
      s
    )
  }
}

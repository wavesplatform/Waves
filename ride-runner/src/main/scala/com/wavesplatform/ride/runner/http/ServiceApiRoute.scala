package com.wavesplatform.ride.runner.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http.*
import com.wavesplatform.ride.runner.http.ServiceApiRoute.Settings

case class ServiceApiRoute(settings: Settings, getServiceStatus: () => HttpServiceStatus) extends ApiRoute {
  override val route: Route = pathPrefix("ride") { status }

  def status: Route = path("status") {
    val s = getServiceStatus()
    complete(
      if (s.healthy) StatusCodes.OK else StatusCodes.InternalServerError,
      s
    )
  }
}

object ServiceApiRoute {
  case class Settings(apiKeyHash: String)
}

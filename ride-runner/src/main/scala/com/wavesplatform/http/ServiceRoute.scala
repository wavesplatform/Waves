package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http.*
import com.wavesplatform.jvm.HeapDumps

case class ServiceRoute(getServiceStatus: () => HttpServiceStatus) extends ApiRoute {
  override val route: Route = pathPrefix("ride") {
    status ~ heapDump
  }

  def status: Route = path("status") {
    val s = getServiceStatus()
    complete(
      if (s.healthy) StatusCodes.OK else StatusCodes.InternalServerError,
      s
    )
  }

  def heapDump: Route = (post & path("heap-dump") & parameter("live".as[Boolean] ? true)) { live =>
    HeapDumps.mk("http", live)
    complete("In progress")
  }
}

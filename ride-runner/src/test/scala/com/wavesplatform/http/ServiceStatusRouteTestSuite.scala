package com.wavesplatform.http

import akka.http.scaladsl.model.{HttpRequest, StatusCodes}

class ServiceStatusRouteTestSuite extends RouteSpec("/utils") with RestAPISettingsHelper {
  "ServiceStatusRoute" - {
    "GET /ride/status" - {
      val request: HttpRequest = Get("/ride/status")

      val healthyRoute = seal(
        ServiceStatusRoute(() => HttpServiceStatus(healthy = true)).route
      )

      "HttpStatus is OK when the service is healthy" in request ~> healthyRoute ~> check {
        status shouldBe StatusCodes.OK
      }

      val unhealthyRoute = seal(
        ServiceStatusRoute(() => HttpServiceStatus(healthy = false)).route
      )

      "HttpStatus is InternalServerError when the service is unhealthy" in request ~> unhealthyRoute ~> check {
        status shouldBe StatusCodes.InternalServerError
      }
    }
  }
}

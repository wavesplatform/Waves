package com.wavesplatform.http

import akka.http.scaladsl.model.{HttpRequest, StatusCodes}
import com.wavesplatform.ride.runner.http.{HttpServiceStatus, ServiceApiRoute}

class ServiceApiRouteTestSuite extends RouteSpec("/utils") with RestAPISettingsHelper {
  "ServiceStatusRoute" - {
    "GET /ride/status" - {
      val request: HttpRequest = Get("/ride/status")

      val healthyRoute = seal(
        ServiceApiRoute(() => HttpServiceStatus(healthy = true)).route
      )

      "HttpStatus is OK when the service is healthy" in request ~> healthyRoute ~> check {
        status shouldBe StatusCodes.OK
      }

      val unhealthyRoute = seal(
        ServiceApiRoute(() => HttpServiceStatus(healthy = false)).route
      )

      "HttpStatus is InternalServerError when the service is unhealthy" in request ~> unhealthyRoute ~> check {
        status shouldBe StatusCodes.InternalServerError
      }
    }
  }
}

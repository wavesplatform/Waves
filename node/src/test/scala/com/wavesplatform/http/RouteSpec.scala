package com.wavesplatform.http

import akka.http.scaladsl.server.{ExceptionHandler, Route}
import akka.http.scaladsl.testkit._
import com.wavesplatform.api.http.CompositeHttpService
import org.scalatest.{FreeSpec, Matchers}

abstract class RouteSpec(basePath: String) extends FreeSpec with ScalatestRouteTest with Matchers with ApiErrorMatchers {
  protected implicit val exceptionHandler: ExceptionHandler = CompositeHttpService.exceptionHandler
  protected def seal(route: Route): Route = Route.seal(route)

  protected def routePath(suffix: String) = s"$basePath$suffix"
}

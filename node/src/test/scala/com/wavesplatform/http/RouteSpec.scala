package com.wavesplatform.http

import akka.http.scaladsl.server.{ExceptionHandler, Route}
import akka.http.scaladsl.testkit._
import com.wavesplatform.api.http
import com.wavesplatform.test._
import com.wavesplatform.utils.JsonMatchers

abstract class RouteSpec(basePath: String) extends FreeSpec with ScalatestRouteTest with ApiErrorMatchers with JsonMatchers {
  protected implicit val exceptionHandler: ExceptionHandler = http.uncaughtExceptionHandler
  protected def seal(route: Route): Route                   = Route.seal(route)

  protected def routePath(suffix: String) = s"$basePath$suffix"
}

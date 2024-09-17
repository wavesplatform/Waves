package com.wavesplatform.http

import akka.http.scaladsl.model.{ContentTypes, FormData, HttpEntity}
import akka.http.scaladsl.server.{ExceptionHandler, Route}
import akka.http.scaladsl.testkit.*
import com.wavesplatform.api.http
import com.wavesplatform.api.http.ApiMarshallers
import com.wavesplatform.test.*
import com.wavesplatform.utils.JsonMatchers
import play.api.libs.json.Json

import scala.concurrent.duration.DurationInt

abstract class RouteSpec(basePath: String) extends FreeSpec with ScalatestRouteTest with ApiErrorMatchers with JsonMatchers with ApiMarshallers {
  protected implicit val routeTestTimeout: RouteTestTimeout = RouteTestTimeout(10 second)
  protected implicit val exceptionHandler: ExceptionHandler = http.uncaughtExceptionHandler
  protected def seal(route: Route): Route                   = Route.seal(route)

  protected def routePath(suffix: String) = s"$basePath$suffix"

  implicit class RouteTestingOps(route: Route) {

    /**
      * Convenient utility for testing multi-routes created with the [[com.wavesplatform.api.http.CustomDirectives#anyParam(java.lang.String)]] directive
      * @param baseUrl Base route URL
      * @param paramName Parameter name
      * @param values Values passed to routes
      * @param doCheck Check response function
      */
    def anyParamTest(baseUrl: String, paramName: String)(values: String*)(doCheck: => Unit): Unit = {
      withClue(s"$baseUrl GET with query params")(Get(s"$baseUrl?${values.map(v => s"$paramName=$v").mkString("&")}") ~> route ~> check {
        doCheck
      })

      withClue(s"$baseUrl POST with form data")(Post(baseUrl, FormData(values.map(paramName -> _) *)) ~> route ~> check {
        doCheck
      })

      withClue(s"$baseUrl POST with json data")(
        Post(baseUrl, HttpEntity(ContentTypes.`application/json`, Json.obj(s"${paramName}s" -> values).toString())) ~> route ~> check {
          doCheck
        }
      )
    }
  }
}

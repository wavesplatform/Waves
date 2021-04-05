package com.wavesplatform.http

import scala.util.Try

import akka.http.scaladsl.server.{ExceptionHandler, Route}
import akka.http.scaladsl.testkit._
import com.wavesplatform.api.http
import org.scalatest.{Assertion, FreeSpec, Matchers}
import play.api.libs.json.{Json, JsValue}

abstract class RouteSpec(basePath: String) extends FreeSpec with ScalatestRouteTest with Matchers with ApiErrorMatchers {
  protected implicit val exceptionHandler: ExceptionHandler = http.uncaughtExceptionHandler
  protected def seal(route: Route): Route                   = Route.seal(route)

  protected def routePath(suffix: String) = s"$basePath$suffix"

  implicit class JsValueAssertOps(value: JsValue) {
    def shouldBeJson(validJson: JsValue): Assertion = {
      val result    = Try(value shouldBe validJson)
      result.failed.foreach(_ => System.err.println(s"Actual json is \n${Json.prettyPrint(value)}"))
      result.get
    }
    
    def shouldBeJson(str: String): Assertion = {
      val validJson = Json.parse(str)
      this.shouldBeJson(validJson)
    }
  }
}

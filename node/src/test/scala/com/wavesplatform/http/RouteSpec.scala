package com.wavesplatform.http

import akka.http.scaladsl.server.{ExceptionHandler, Route}
import akka.http.scaladsl.testkit._
import com.wavesplatform.api.http
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.matchers.{Matcher, MatchResult}
import play.api.libs.json.{Json, JsValue}

abstract class RouteSpec(basePath: String) extends FreeSpec with ScalatestRouteTest with Matchers with ApiErrorMatchers {
  protected implicit val exceptionHandler: ExceptionHandler = http.uncaughtExceptionHandler
  protected def seal(route: Route): Route                   = Route.seal(route)

  protected def routePath(suffix: String) = s"$basePath$suffix"

  def matchJson(value: JsValue): JsonWord = JsonWord(value)
  def matchJson(value: String): JsonWord = matchJson(Json.parse(value))

  case class JsonWord(value: JsValue) extends Matcher[JsValue] {
    def apply(left: JsValue): MatchResult = {
      MatchResult(
        left == value,
        s"${Json.prettyPrint(left)}\n was not equal to \n${Json.prettyPrint(value)}",
        s"${Json.prettyPrint(left)}\n was equal to \n${Json.prettyPrint(value)}"
      )
    }
  }
}

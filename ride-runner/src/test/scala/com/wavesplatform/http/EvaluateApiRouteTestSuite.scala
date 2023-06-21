package com.wavesplatform.http

import akka.http.scaladsl.model.{HttpRequest, StatusCodes}
import com.wavesplatform.ride.runner.http.EvaluateApiRoute
import com.wavesplatform.ride.runner.requests.RideScriptRunResult
import com.wavesplatform.wallet.Wallet
import play.api.libs.json.*

import java.nio.charset.StandardCharsets
import scala.concurrent.Future

class EvaluateApiRouteTestSuite extends RouteSpec("/utils") with RestAPISettingsHelper {
  private val default     = Wallet.generateNewAccount("test".getBytes(StandardCharsets.UTF_8), 0)
  private val defaultAddr = default.toAddress

  "EvaluateApiRoute" - {
    "POST /utils/script/evaluate/{address}" - {
      val okApi = EvaluateApiRoute { request =>
        Future.successful(
          RideScriptRunResult(
            evaluation = None,
            lastResult = Json
              .obj(
                "result" -> Json.obj(
                  "type"  -> "Int",
                  "value" -> 2
                ),
                "complexity" -> 0,
                "vars"       -> Json.arr(Json.obj("a" -> 1)),
                "expr"       -> "1 + 1",
                "address"    -> defaultAddr.toString
              )
              .toString(),
            lastStatus = StatusCodes.OK
          )
        )
      }
      val okRoute = seal(okApi.route)

      def evalScript(text: String): HttpRequest =
        Post(s"/utils/script/evaluate/$defaultAddr", Json.obj("expr" -> text))

      "happy path" in evalScript("1 + 1") ~> okRoute ~> check {
        status shouldBe StatusCodes.OK
        responseAs[JsObject].value should not be empty
      }

      val failApi = EvaluateApiRoute { request =>
        Future.successful(
          RideScriptRunResult(
            evaluation = None,
            lastResult = JsObject.empty.toString(),
            lastStatus = StatusCodes.BadRequest
          )
        )
      }

      val failRoute = seal(failApi.route)
      "an user error" in evalScript("1 + 1") ~> failRoute ~> check {
        status shouldBe StatusCodes.BadRequest
      }
    }
  }
}

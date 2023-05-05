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
      val okApi = EvaluateApiRoute({ request =>
        Future.successful(
          RideScriptRunResult(
            request = request,
            lastResult = Json.obj(
              "result" -> Json.obj(
                "type"  -> "Int",
                "value" -> 2
              ),
              "complexity" -> 0,
              "vars"       -> Json.arr(),
              "expr"       -> "1 + 1",
              "address"    -> defaultAddr.toString
            ),
            lastStatus = StatusCodes.OK,
            updateHeight = 1
          )
        )
      })
      val okRoute = seal(okApi.route)

      def evalScript(text: String, trace: Boolean): HttpRequest =
        Post(s"/utils/script/evaluate/$defaultAddr${if (trace) "?trace=true" else ""}", Json.obj("expr" -> text))

      "without traces" in evalScript("1 + 1", trace = false) ~> okRoute ~> check {
        status shouldBe StatusCodes.OK
        responseAs[JsObject] \\ "vars" shouldBe empty
      }

      "with traces" in evalScript("1 + 1", trace = true) ~> okRoute ~> check {
        status shouldBe StatusCodes.OK
        responseAs[JsObject] \\ "vars" should not be empty
      }

      val failApi = EvaluateApiRoute({ request =>
        Future.successful(
          RideScriptRunResult(
            request = request,
            lastResult = JsObject.empty,
            lastStatus = StatusCodes.BadRequest,
            updateHeight = 1
          )
        )
      })

      val failRoute = seal(failApi.route)
      "user error" in evalScript("1 + 1", trace = true) ~> failRoute ~> check {
        status shouldBe StatusCodes.BadRequest
      }
    }
  }
}

package com.wavesplatform.http

import akka.http.scaladsl.model.HttpRequest
import com.wavesplatform.ride.runner.http.EvaluateApiRoute
import com.wavesplatform.wallet.Wallet
import play.api.libs.json.*

import java.nio.charset.StandardCharsets
import scala.concurrent.Future

class EvaluateApiRouteTestSuite extends RouteSpec("/utils") with RestAPISettingsHelper {
  private val default     = Wallet.generateNewAccount("test".getBytes(StandardCharsets.UTF_8), 0)
  private val defaultAddr = default.toAddress

  "EvaluateApiRoute" - {
    "POST /utils/script/evaluate/{address}" - {
      val api = EvaluateApiRoute({ x =>
        Future.successful(
          Json.obj(
            "result" -> Json.obj(
              "type"  -> "Int",
              "value" -> 2
            ),
            "complexity" -> 0,
            "vars"       -> Json.arr(),
            "expr"       -> "1 + 1",
            "address"    -> defaultAddr.toString
          )
        )
      })
      val route = seal(api.route)

      def evalScript(text: String, trace: Boolean): HttpRequest =
        Post(s"/utils/script/evaluate/$defaultAddr${if (trace) "?trace=true" else ""}", Json.obj("expr" -> text))

      "without traces" in evalScript("1 + 1", trace = false) ~> route ~> check {
        responseAs[JsObject] \\ "vars" shouldBe empty
      }

      "with traces" in evalScript("1 + 1", trace = true) ~> route ~> check {
        responseAs[JsObject] \\ "vars" should not be empty
      }
    }
  }
}

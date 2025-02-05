package com.wavesplatform.api

import cats.syntax.either.*
import com.wavesplatform.account.Address
import com.wavesplatform.api.RideMulticastHttpApi.{AskResult, AskServerResponse, Settings}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import play.api.libs.json.{JsObject, Json}
import sttp.client3.*

import scala.concurrent.duration.{Duration, DurationLong, FiniteDuration}

class RideMulticastHttpApi(settings: Settings, httpBackend: SttpBackend[Identity, Any]) extends ScorexLogging {
  def ask(address: Address, request: JsObject, trace: Boolean = false): Task[AskResult] = Task
    .parZip2(
      Task(evaluate(settings.rideRunnerApiBaseUri, address, request, trace)),
      Task(evaluate(settings.nodeApiBaseUri, address, request, trace))
    )
    .map(Function.tupled(AskResult.apply))

  private def evaluate(baseUri: String, address: Address, request: JsObject, trace: Boolean): AskServerResponse = {
    val start = System.nanoTime()
    val r = basicRequest
      .post(uri"$baseUri/utils/script/evaluate/$address".withParam("trace", Some(trace.toString)))
      .body(Json.stringify(request))
      .header("Content-Type", "application/json")
      .response(asString)
      .send(httpBackend)
      .body
      .flatMap { rawJson =>
        Json.parse(rawJson) match {
          case x: JsObject => x.asRight
          case x           => s"Expected a JsObject, but got:\n$x".asLeft
        }
      }
    AskServerResponse(r, System.nanoTime() - start)
  }
}

object RideMulticastHttpApi {
  case class Settings(rideRunnerApiBaseUri: String, nodeApiBaseUri: String)

  case class AskResult(rideRunner: AskServerResponse, node: AskServerResponse) {
    def timeDiff: FiniteDuration = (node.time - rideRunner.time).toCoarsest
  }

  case class AskServerResponse(response: Either[String, JsObject], nanoTime: Long) {
    def time: FiniteDuration = Duration.fromNanos(nanoTime).toMillis.millis.toCoarsest
  }
}

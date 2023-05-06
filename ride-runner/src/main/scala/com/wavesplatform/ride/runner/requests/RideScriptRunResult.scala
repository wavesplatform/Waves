package com.wavesplatform.ride.runner.requests

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.wavesplatform.api.http.utils.UtilsEvaluator.Evaluation
import play.api.libs.json.JsObject

final case class RideScriptRunResult(
    request: RideScriptRunRequest,
    evaluation: Option[Evaluation],
    lastResult: JsObject,
    lastStatus: StatusCode
)

object RideScriptRunResult {
  def apply(key: RideScriptRunRequest): RideScriptRunResult = RideScriptRunResult(
    key,
    None,
    JsObject.empty,
    StatusCodes.InternalServerError
  )
}

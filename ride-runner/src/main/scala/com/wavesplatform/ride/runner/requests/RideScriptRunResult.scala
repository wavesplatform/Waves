package com.wavesplatform.ride.runner.requests

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.wavesplatform.ride.runner.storage.RideScriptRunRequest
import play.api.libs.json.JsObject

final case class RideScriptRunResult(request: RideScriptRunRequest, lastResult: JsObject, lastStatus: StatusCode, updateHeight: Int)

object RideScriptRunResult {
  def apply(key: RideScriptRunRequest): RideScriptRunResult = RideScriptRunResult(key, JsObject.empty, StatusCodes.InternalServerError, 0)
}

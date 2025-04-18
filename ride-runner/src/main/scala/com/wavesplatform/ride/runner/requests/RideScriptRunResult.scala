package com.wavesplatform.ride.runner.requests

import org.apache.pekko.http.scaladsl.model.{StatusCode, StatusCodes}
import com.wavesplatform.api.http.utils.Evaluation

final case class RideScriptRunResult(
    evaluation: Option[Evaluation],
    lastResult: String,
    lastStatus: StatusCode
)

object RideScriptRunResult {
  def apply(): RideScriptRunResult = RideScriptRunResult(
    None,
    "",
    StatusCodes.InternalServerError
  )
}

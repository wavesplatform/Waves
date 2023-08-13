package com.wavesplatform.ride.runner.input

import com.wavesplatform.json.JsonManipulations
import play.api.libs.json.{JsObject, JsValue}

sealed trait RideRunnerPostProcessingMethod {
  def process(js: JsValue): JsValue
}

case class PickRideRunnerPostProcessingMethod(path: String) extends RideRunnerPostProcessingMethod {
  override def process(js: JsValue): JsValue = JsonManipulations.pick(js, path).getOrElse(JsObject.empty)
}

case class PickAllRideRunnerPostProcessingMethod(paths: List[String]) extends RideRunnerPostProcessingMethod {
  override def process(js: JsValue): JsValue = JsonManipulations.pickAll(js, paths)
}

case class PruneRideRunnerPostProcessingMethod(paths: List[String]) extends RideRunnerPostProcessingMethod {
  override def process(js: JsValue): JsValue = JsonManipulations.pruneAll(js, paths)
}

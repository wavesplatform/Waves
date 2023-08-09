package com.wavesplatform.ride.runner.input

import com.wavesplatform.json.JsonManipulations
import play.api.libs.json.{JsArray, JsObject, JsValue}

sealed trait RideRunnerPostProcessingMethod {
  def process(js: JsValue): JsValue
}

case class PickRideRunnerPostProcessingMethod(path: String) extends RideRunnerPostProcessingMethod {
  override def process(js: JsValue): JsValue = JsonManipulations.pick(js, path).getOrElse(JsObject.empty)
}

case class PickAllRideRunnerPostProcessingMethod(paths: List[String]) extends RideRunnerPostProcessingMethod {
  override def process(js: JsValue): JsValue = JsArray(
    paths
      .foldLeft(List.empty[JsValue]) { (r, path) =>
        JsonManipulations.pick(js, path) match {
          case None    => r
          case Some(x) => x :: r
        }
      }
      .reverse
  )
}

case class PruneRideRunnerPostProcessingMethod(paths: List[String]) extends RideRunnerPostProcessingMethod {
  override def process(js: JsValue): JsValue = JsonManipulations.pruneAll(js, paths)
}

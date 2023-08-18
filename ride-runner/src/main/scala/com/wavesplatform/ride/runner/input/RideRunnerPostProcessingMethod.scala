package com.wavesplatform.ride.runner.input

import com.wavesplatform.json.JsonManipulations
import play.api.libs.json.{JsObject, JsValue}

sealed trait RideRunnerPostProcessingMethod {
  def process(js: JsValue): JsValue
}

object RideRunnerPostProcessingMethod {
  case class Pick(path: String) extends RideRunnerPostProcessingMethod {
    override def process(js: JsValue): JsValue = JsonManipulations.pick(js, path).getOrElse(JsObject.empty)
  }

  case class PickAll(paths: List[String]) extends RideRunnerPostProcessingMethod {
    override def process(js: JsValue): JsValue = JsonManipulations.pickAll(js, paths)
  }

  case class Prune(paths: List[String]) extends RideRunnerPostProcessingMethod {
    override def process(js: JsValue): JsValue = JsonManipulations.pruneAll(js, paths)
  }

  case class Regex(path: String, find: String, replace: String) extends RideRunnerPostProcessingMethod {
    override def process(js: JsValue): JsValue = JsonManipulations.regexReplace(js, path, find, replace) match {
      case Right(r) => r
      case Left(e)  => throw new IllegalArgumentException(e)
    }
  }
}

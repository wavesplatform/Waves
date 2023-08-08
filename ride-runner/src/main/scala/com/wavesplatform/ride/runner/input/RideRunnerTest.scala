package com.wavesplatform.ride.runner.input

import play.api.libs.json.*

case class RideRunnerTest(
    paths: List[String] = List("result.value._2.value"),
    method: String = "pick", // prune | p TODO type
    result: JsValue
)

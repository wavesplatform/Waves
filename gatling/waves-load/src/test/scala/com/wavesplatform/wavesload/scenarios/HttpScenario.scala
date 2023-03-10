package com.wavesplatform.wavesload.scenarios

import io.gatling.core.Predef._
import io.gatling.core.structure.ScenarioBuilder
import com.wavesplatform.wavesload.cases._

object HttpScenario {
  def apply(): ScenarioBuilder = new HttpScenario().scn
}

class HttpScenario {

  val scn: ScenarioBuilder = scenario("Http Scenario")
    .exec(HttpActions.getMainPage)

}

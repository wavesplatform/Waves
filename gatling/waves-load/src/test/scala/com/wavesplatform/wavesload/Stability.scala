package com.wavesplatform.wavesload

import io.gatling.core.Predef._
import ru.tinkoff.gatling.config.SimulationConfig._
import ru.tinkoff.gatling.influxdb.Annotations
import com.wavesplatform.wavesload.scenarios._

class Stability extends Simulation with Annotations {

  setUp(
    HttpScenario().inject(
      // разгон
      rampUsersPerSec(0) to intensity.toInt during rampDuration,
      // полка
      constantUsersPerSec(intensity.toInt) during stageDuration,
    ),
  ).protocols(
    httpProtocol,
    // длительность теста = разгон + полка
  ).maxDuration(testDuration)

}

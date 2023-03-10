package com.wavesplatform.wavesload

import io.gatling.http.Predef._
import io.gatling.core.Predef._
import ru.tinkoff.gatling.config.SimulationConfig._
import com.wavesplatform.wavesload.scenarios._

class Debug extends Simulation {

  // proxy is required on localhost:8888

  setUp(
    HttpScenario().inject(atOnceUsers(1)),
  ).protocols(
    httpProtocol,
    //        .proxy(Proxy("localhost", 8888).httpsPort(8888))
  ).maxDuration(testDuration)

}

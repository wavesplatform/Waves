package com.wavesplatform

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import ru.tinkoff.gatling.config.SimulationConfig._
package object wavesload {

  // common http protocol params (eg headers, checks)
  val httpProtocol = http
    .baseUrl(
      baseUrl,
    )                                                                                // Here is the root for all relative URLs, located in simulation.conf file, or -DbaseUrl="" passed to test param
    .acceptHeader("text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8") // Here are the common headers
    .acceptEncodingHeader("gzip, deflate")
    .acceptLanguageHeader("en-US,en;q=0.5")
    .userAgentHeader("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:16.0) Gecko/20100101 Firefox/16.0")
    .disableFollowRedirect

}

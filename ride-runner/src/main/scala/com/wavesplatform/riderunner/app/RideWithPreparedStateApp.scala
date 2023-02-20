package com.wavesplatform.riderunner.app

import com.wavesplatform.api.http.utils.UtilsApiRoute
import com.wavesplatform.blockchain.ImmutableBlockchain
import com.wavesplatform.riderunner.input.RideRunnerInput
import play.api.libs.json.Json

import java.io.File
import scala.io.Source
import scala.util.Using

object RideWithPreparedStateApp {
  def main(args: Array[String]): Unit = {
    if (args.length < 2) throw new IllegalArgumentException("Usage: <path-to-app.conf> <path-to-input.json>")

    val configPath    = args(0)
    val inputJsonPath = args(1)

    val (_, globalSettings) = AppInitializer.init(Some(new File(configPath)))

    val input = RideRunnerInput.parse(Using(Source.fromFile(new File(inputJsonPath)))(_.getLines().mkString("\n")).get)

    val blockchain = new ImmutableBlockchain(globalSettings.rideRunner.immutableBlockchain, input)

    val apiResult = UtilsApiRoute.evaluate(
      evaluateScriptComplexityLimit = globalSettings.rideRunner.requestsService.evaluateScriptComplexityLimit,
      blockchain = blockchain,
      address = input.address,
      request = input.request,
      trace = input.trace,
      maxTxErrorLogSize = globalSettings.rideRunner.requestsService.maxTxErrorLogSize
    )

    println(Json.prettyPrint(apiResult))
  }
}

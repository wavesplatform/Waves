package com.wavesplatform.ride.runner.entrypoints

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import com.wavesplatform.api.http.utils.UtilsEvaluator
import com.wavesplatform.ride.runner.blockchain.ImmutableBlockchain
import com.wavesplatform.ride.runner.input.RideRunnerJson
import play.api.libs.json.Json

import java.io.File
import scala.io.Source
import scala.util.Using

object RideRunnerWithPreparedStateApp {
  def main(args: Array[String]): Unit = {
    if (args.length < 2) throw new IllegalArgumentException("Usage: <path-to-app.conf> <path-to-input.json/conf>")

    val configPath    = args(0)
    val inputJsonPath = args(1)

    val (_, globalSettings) = AppInitializer.init(checkDb = false, externalConfig = Some(new File(configPath)))

    val inputFile     = new File(inputJsonPath)
    val inputFileName = inputFile.getName
    val json =
      if (inputFileName.endsWith(".conf")) ConfigFactory.parseFile(inputFile).resolve().root().render(ConfigRenderOptions.concise())
      else if (inputFileName.endsWith(".json")) Using(Source.fromFile(new File(inputJsonPath)))(_.getLines().mkString("\n")).get
      else throw new IllegalArgumentException("Expected JSON or HOCON file")

    val input      = RideRunnerJson.parse(json)
    val blockchain = new ImmutableBlockchain(globalSettings.rideRunner.immutableBlockchain, input)

    val apiResult = UtilsEvaluator.evaluate(
      evaluateScriptComplexityLimit = globalSettings.rideRunner.requestsService.evaluateScriptComplexityLimit,
      blockchain = blockchain,
      address = input.address,
      request = input.request,
      trace = input.trace,
      maxTxErrorLogSize = globalSettings.rideRunner.requestsService.maxTxErrorLogSize,
      intAsString = true // TODO #110 Int as string in evaluate
    )

    println(Json.prettyPrint(apiResult))
  }
}

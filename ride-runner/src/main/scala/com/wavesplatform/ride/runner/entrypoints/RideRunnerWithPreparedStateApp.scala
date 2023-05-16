package com.wavesplatform.ride.runner.entrypoints

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import com.wavesplatform.Version
import com.wavesplatform.api.http.utils.UtilsEvaluator
import com.wavesplatform.ride.runner.blockchain.ImmutableBlockchain
import com.wavesplatform.ride.runner.input.RideRunnerJson
import play.api.libs.json.Json
import scopt.{DefaultOParserSetup, OParser, OParserSetup}

import java.io.File
import scala.io.Source
import scala.util.Using

object RideRunnerWithPreparedStateApp {
  def main(args: Array[String]): Unit = {
    val setup: OParserSetup = new DefaultOParserSetup {
      override def showUsageOnError = Some(true)
    }

    OParser.parse(commandParser, args, Args(), setup).foreach { args =>
      val (_, globalSettings) = AppInitializer.init(checkDb = false, externalConfig = Some(args.configFile))

      val inputFileName = args.inputFile.getName
      val json =
        if (inputFileName.endsWith(".conf")) ConfigFactory.parseFile(args.inputFile).resolve().root().render(ConfigRenderOptions.concise())
        else if (inputFileName.endsWith(".json")) Using(Source.fromFile(args.inputFile))(_.getLines().mkString("\n")).get
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

  private final case class Args(
      configFile: File = new File(""),
      inputFile: File = new File("")
  )

  private val commandParser = {
    import scopt.OParser

    val builder = OParser.builder[Args]
    import builder.*

    OParser.sequence(
      head("RIDE script runner", Version.VersionString),
      opt[File]('c', "config")
        .text("Path to a config. Alternatively you can add settings in the input file")
        .required()
        .action((f, c) => c.copy(configFile = f)),
      opt[File]('i', "input")
        .text("Path to JSON or HOCON (conf) file with prepared state and run arguments")
        .required()
        .action((f, c) => c.copy(inputFile = f)),
      help("help").hidden()
    )
  }
}

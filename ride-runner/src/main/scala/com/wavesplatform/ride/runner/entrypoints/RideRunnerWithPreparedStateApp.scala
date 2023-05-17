package com.wavesplatform.ride.runner.entrypoints

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import com.wavesplatform.Version
import com.wavesplatform.api.http.utils.UtilsEvaluator
import com.wavesplatform.ride.runner.blockchain.ImmutableBlockchain
import com.wavesplatform.ride.runner.input.RideRunnerInputParser
import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, GenesisSettings, RewardsSettings}
import play.api.libs.json.*
import scopt.{DefaultOParserSetup, OParser, OParserSetup}

import java.io.File
import scala.concurrent.duration.DurationInt
import scala.io.Source
import scala.util.Using

object RideRunnerWithPreparedStateApp {
  def main(args: Array[String]): Unit = {
    val setup: OParserSetup = new DefaultOParserSetup {
      override val showUsageOnError = Some(true)
    }

    OParser.parse(commandParser, args, Args(), setup).foreach { args =>
      System.setProperty("logback.stdout.level", if (args.verbose) "TRACE" else "OFF")

      val inputFileName = args.inputFile.getName
      val inputRawJson =
        if (inputFileName.endsWith(".conf")) ConfigFactory.parseFile(args.inputFile).resolve().root().render(ConfigRenderOptions.concise())
        else if (inputFileName.endsWith(".json")) Using(Source.fromFile(args.inputFile))(_.getLines().mkString("\n")).get
        else throw new IllegalArgumentException("Expected JSON or HOCON file")

      val inputJson = RideRunnerInputParser.parseJson(inputRawJson)
      AppInitializer.setupChain(RideRunnerInputParser.getChainId(inputJson)) // We must setup chain first to parse addresses
      val input = RideRunnerInputParser.parse(inputJson)

      val blockchain = new ImmutableBlockchain(
        settings = BlockchainSettings(
          addressSchemeCharacter = input.chainId,
          functionalitySettings = FunctionalitySettings(),
          genesisSettings = GenesisSettings(0, 0, 0, None, Nil, 0, 0.seconds),
          rewardsSettings = RewardsSettings(1, 0, 1, 1)
        ),
        input
      )

      val apiResult = UtilsEvaluator.evaluate(
        evaluateScriptComplexityLimit = input.evaluateScriptComplexityLimit,
        blockchain = blockchain,
        address = input.address,
        request = input.request,
        trace = input.trace,
        maxTxErrorLogSize = input.maxTxErrorLogSize,
        intAsString = true // TODO #110 Int as string in evaluate
      )

      println(Json.prettyPrint(apiResult))
    }
  }

  private final case class Args(
      inputFile: File = new File(""),
      verbose: Boolean = false
  )

  private val commandParser = {
    import scopt.OParser

    val builder = OParser.builder[Args]
    import builder.*

    OParser.sequence(
      head("RIDE script runner", Version.VersionString),
      opt[File]('i', "input")
        .text("Path to JSON or HOCON (conf) file with prepared state and run arguments. It has highest priority than config.")
        .required()
        .action((x, c) => c.copy(inputFile = x)),
      opt[Unit]('v', "verbose")
        .text("Print logs")
        .action((x, c) => c.copy(verbose = true)),
      help("help").hidden()
    )
  }
}

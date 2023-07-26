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

      val runResult = run(inputJson)
      println(Json.prettyPrint(runResult))
    }
  }

  def run(inputJson: JsValue): JsObject = {
    val input = RideRunnerInputParser.parse(inputJson)
    val defaultFunctionalitySettings = input.chainId match {
      case 'W' => BlockchainSettings(input.chainId, FunctionalitySettings.MAINNET, GenesisSettings.MAINNET, RewardsSettings.MAINNET)
      case 'T' => BlockchainSettings(input.chainId, FunctionalitySettings.TESTNET, GenesisSettings.TESTNET, RewardsSettings.TESTNET)
      case 'S' => BlockchainSettings(input.chainId, FunctionalitySettings.STAGENET, GenesisSettings.STAGENET, RewardsSettings.STAGENET)
      case _ =>
        BlockchainSettings(
          addressSchemeCharacter = input.chainId,
          functionalitySettings = FunctionalitySettings(),
          genesisSettings = GenesisSettings(0, 0, 0, None, Nil, 0, 0.seconds),
          rewardsSettings = RewardsSettings(1, 1, 1, 1, 1)
        )
    }

    UtilsEvaluator.evaluate(
      blockchain = new ImmutableBlockchain(defaultFunctionalitySettings, input.state),
      dAppAddress = input.address,
      request = input.request,
      options = UtilsEvaluator.EvaluateOptions(
        evaluateScriptComplexityLimit = input.evaluateScriptComplexityLimit,
        maxTxErrorLogSize = input.maxTxErrorLogSize,
        enableTraces = input.trace,
        intAsString = input.intAsString
      )
    )
  }

  private final case class Args(
      private val rawInputFile: File = new File(""),
      verbose: Boolean = false
  ) {
    val inputFile = rawInputFile.getAbsoluteFile // Otherwise Lightbend Config doesn't resolve correctly relative paths
  }

  private val commandParser = {
    import scopt.OParser

    val builder = OParser.builder[Args]
    import builder.*

    OParser.sequence(
      head("RIDE script runner", Version.VersionString),
      opt[File]('i', "input")
        .text("Path to JSON or HOCON (conf) file with prepared state and run arguments. It has highest priority than the config.")
        .required()
        .action((x, c) => c.copy(rawInputFile = x)),
      opt[Unit]('v', "verbose")
        .text("Print logs")
        .action((x, c) => c.copy(verbose = true)),
      help("help").hidden()
    )
  }
}

package com.wavesplatform.ride.runner.entrypoints

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import com.wavesplatform.Version
import com.wavesplatform.api.http.utils.{Evaluation, UtilsEvaluator}
import com.wavesplatform.lang.directives.values.StdLibVersion
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

      val defaultFunctionalitySettings = input.chainId match {
        case 'W' => BlockchainSettings(input.chainId, FunctionalitySettings.MAINNET, GenesisSettings.MAINNET, RewardsSettings.MAINNET)
        case 'T' => BlockchainSettings(input.chainId, FunctionalitySettings.TESTNET, GenesisSettings.TESTNET, RewardsSettings.TESTNET)
        case 'S' => BlockchainSettings(input.chainId, FunctionalitySettings.STAGENET, GenesisSettings.STAGENET, RewardsSettings.STAGENET)
        case _ =>
          BlockchainSettings(
            addressSchemeCharacter = input.chainId,
            functionalitySettings = FunctionalitySettings(),
            genesisSettings = GenesisSettings(0, 0, 0, None, Nil, 0, 0.seconds),
            rewardsSettings = RewardsSettings(1, 0, 1, 1, 1000)
          )
      }

      val runResult = Evaluation.parse(StdLibVersion.VersionDic.latest, input.address, input.request) match {
        case Left(e) => UtilsEvaluator.validationErrorToJson(e, input.maxTxErrorLogSize)
        case Right(evaluation) =>
          val blockchain = new ImmutableBlockchain(defaultFunctionalitySettings, input)
          val address    = input.address
          val scriptInfo = blockchain.accountScript(address).getOrElse(throw new RuntimeException(s"There is no script on '$address'"))
          UtilsEvaluator.evaluate(
            evaluateScriptComplexityLimit = input.evaluateScriptComplexityLimit,
            blockchain = blockchain,
            scriptInfo = scriptInfo,
            evaluation = evaluation,
            address = address,
            trace = input.trace,
            maxTxErrorLogSize = input.maxTxErrorLogSize,
            intAsString = input.intAsString,
            wrapDAppEnv = identity
          )
      }

      println(Json.prettyPrint(runResult))
    }
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
        .text("Path to JSON or HOCON (conf) file with prepared state and run arguments. It has highest priority than config.")
        .required()
        .action((x, c) => c.copy(rawInputFile = x)),
      opt[Unit]('v', "verbose")
        .text("Print logs")
        .action((x, c) => c.copy(verbose = true)),
      help("help").hidden()
    )
  }
}

package com.wavesplatform.ride.runner.entrypoints

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import com.wavesplatform.Version
import com.wavesplatform.api.http.utils.UtilsEvaluator
import com.wavesplatform.ride.runner.blockchain.ImmutableBlockchain
import com.wavesplatform.ride.runner.input.RideRunnerJson
import com.wavesplatform.settings.BlockchainSettings
import play.api.libs.json.{JsValue, Json}
import scopt.{DefaultOParserSetup, OParser, OParserSetup}

import java.io.File
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
      val inputJson = RideRunnerJson.parseJson(inputRawJson)

      val settingsFromInput = Settings.fromInputJson(inputJson)
      val settings = (args.configFile, settingsFromInput) match {
        case (None, None) =>
          throw new IllegalArgumentException("You have to specify either --config with 'waves.blockchain' setting or 'settings' field in input file")

        case (_, Some(settings)) =>
          AppInitializer.setupChain(settings.blockchain)
          settings

        case (configFile, _) =>
          val globalConfig = AppInitializer.loadConfig(configFile)

          // Can't use config.getString, because a part of config is hard-coded in BlockchainSettings
          val blockchainSettings = BlockchainSettings.fromRootConfig(globalConfig)
          AppInitializer.setupChain(blockchainSettings)

          RideRunnerGlobalSettings.fromRootConfig(globalConfig).rideRunner.withPreparedStateApp
      }

      val input      = RideRunnerJson.parse(inputJson) // We must setup chain first to parse addresses
      val blockchain = new ImmutableBlockchain(settings.blockchain, input)

      val apiResult = UtilsEvaluator.evaluate(
        evaluateScriptComplexityLimit = settings.evaluateScriptComplexityLimit,
        blockchain = blockchain,
        address = input.address,
        request = input.request,
        trace = input.trace,
        maxTxErrorLogSize = settings.maxTxErrorLogSize,
        intAsString = true // TODO #110 Int as string in evaluate
      )

      println(Json.prettyPrint(apiResult))
    }
  }

  case class Settings(
      blockchain: BlockchainSettings,
      evaluateScriptComplexityLimit: Int = Int.MaxValue,
      maxTxErrorLogSize: Int = 100
  )

  object Settings {
    def fromInputJson(json: JsValue): Option[Settings] =
      (json \ "settings").asOpt[Settings](RideRunnerJson.rideRunnerWithPreparedStateAppSettingsReads)
  }

  private final case class Args(
      configFile: Option[File] = None,
      inputFile: File = new File(""),
      verbose: Boolean = false
  )

  private val commandParser = {
    import scopt.OParser

    val builder = OParser.builder[Args]
    import builder.*

    OParser.sequence(
      head("RIDE script runner", Version.VersionString),
      opt[File]('c', "config")
        .text("Path to a config. Alternatively you can add settings in the input file.")
        .action((x, c) => c.copy(configFile = Some(x))),
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

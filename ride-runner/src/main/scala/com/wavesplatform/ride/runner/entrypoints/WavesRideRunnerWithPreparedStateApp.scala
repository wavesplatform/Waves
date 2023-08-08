package com.wavesplatform.ride.runner.entrypoints

import cats.syntax.either.*
import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import com.wavesplatform.Version
import com.wavesplatform.api.http.utils.UtilsEvaluator
import com.wavesplatform.ride.runner.blockchain.ImmutableBlockchain
import com.wavesplatform.ride.runner.input.RideRunnerInputParser
import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, GenesisSettings, RewardsSettings}
import monix.eval.Task
import monix.execution.Scheduler.global
import monix.reactive.Observable
import org.slf4j.LoggerFactory
import play.api.libs.json.*
import scopt.{DefaultOParserSetup, OParser}

import java.io.File
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}

object WavesRideRunnerWithPreparedStateApp {
  def main(args: Array[String]): Unit = {
    val setup = new DefaultOParserSetup {
      override val showUsageOnError = Some(true)
    }

    OParser.parse(commandParser, args, Args(), setup).foreach { args =>
      System.setProperty("logback.stdout.level", if (args.verbose) "TRACE" else "OFF")
      val log = LoggerFactory.getLogger("Main")

      val r = Observable
        .fromIterable(args.input)
        .mapParallelOrdered(args.parallelism) { file =>
          Task[(File, Either[String, JsObject])] {
            val inputFileName = file.getName
            // TODO do not parse JSON!
            val inputRawJson =
              if (inputFileName.endsWith(".conf")) ConfigFactory.parseFile(file).resolve().root().render(ConfigRenderOptions.concise())
              else throw new IllegalArgumentException(s"Expected $file to be a HOCON file")

            val inputJson = RideRunnerInputParser.parseJson(inputRawJson)
            AppInitializer.setupChain(RideRunnerInputParser.getChainId(inputJson)) // We must setup chain first to parse addresses

            (file, run(inputJson).asRight[String])
          }.onErrorRecover { e =>
            log.warn(s"Error during running $file", e)
            (file, e.getMessage.asLeft[JsObject])
          }
        }
        .foreach {
          case (file, Left(error)) =>
            System.err.println(s"[ERROR] $file: $error")
          case (file, Right(runResult)) =>
            println(s"""[OK] $file:
                       |${Json.prettyPrint(runResult).split('\n').mkString("  ", "\n  ", "\n")}
                       |""".stripMargin)
        }(global)

      Await.result(r, Duration.Inf)
      println("\nDone")
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
      input: List[File] = Nil,
      parallelism: Int = Runtime.getRuntime.availableProcessors(),
      mode: Mode = Mode.Run,
      verbose: Boolean = false
  )

  sealed trait Mode
  object Mode {
    case object Run                             extends Mode
    case class Test(junit: Option[File] = None) extends Mode
  }

  private val commandParser = {
    import scopt.OParser

    val builder = OParser.builder[Args]
    import builder.*

    OParser.sequence(
      head("RIDE script runner", Version.VersionString),
      opt[Unit]('v', "verbose")
        .text("Print logs")
        .action((x, c) => c.copy(verbose = true)),
      opt[Int]('p', "parallelism")
        .optional()
        .text("A number of parallel threads to process scripts. A number of available cores by default")
        .action((x, c) => c.copy(parallelism = x)),
      help("help").hidden(),
      arg[File]("<file>...")
        .unbounded()
        .required()
        .text("Path to HOCON (conf) file (or files) with prepared state and run arguments.")
        .action((x, c) => c.copy(input = x.getAbsoluteFile :: c.input)),
      cmd("run")
        .text("  Run scripts and print results to STDOUT.")
        .action((_, c) => c.copy(mode = Mode.Run)),
      cmd("test")
        .text("  Expects that provided files are tests.")
        .action((_, c) => c.copy(mode = Mode.Test()))
        .children(
          opt[File]("junit")
            .text("Write a JUnit report to this file. An existed file will be overwritten.")
            .action((x, c) => c.copy(mode = Mode.Test(Some(x))))
        )
    )
  }
}

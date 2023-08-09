package com.wavesplatform.ride.runner.entrypoints

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import com.wavesplatform.Version
import com.wavesplatform.api.http.utils.UtilsEvaluator
import com.wavesplatform.io.PathUtils
import com.wavesplatform.ride.runner.blockchain.ImmutableBlockchain
import com.wavesplatform.ride.runner.input.RideRunnerInputParser
import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, GenesisSettings, RewardsSettings}
import monix.eval.Task
import monix.execution.Scheduler.global
import monix.execution.schedulers.CanBlock
import monix.reactive.Observable
import org.slf4j.LoggerFactory
import play.api.libs.json.*
import scopt.{DefaultOParserSetup, OParser}

import java.io.File
import java.nio.file.Path
import scala.concurrent.duration.DurationInt
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success, Try}

object WavesRideRunnerWithPreparedStateApp {
  def main(args: Array[String]): Unit = {
    val setup = new DefaultOParserSetup {
      override val showUsageOnError = Some(true)
    }

    OParser.parse(commandParser, args, Args(), setup).foreach { args =>
      System.setProperty("logback.stdout.level", if (args.verbose) "TRACE" else "OFF")
      val log        = LoggerFactory.getLogger("Main")
      val input      = args.input.map(_.toPath.toAbsolutePath) // Otherwise Lightbend Config doesn't resolve correctly relative paths
      val commonPath = input.reduceLeft(PathUtils.commonPath)

      // TODO if one file
      Observable
        .fromIterable(input)
        .mapParallelOrdered(args.parallelism) { path =>
          Task {
            if (path.getFileName.toString.endsWith(".conf"))
              Try(ConfigFactory.parseFile(path.toFile).resolve().root().render(ConfigRenderOptions.concise())) match {
                case Failure(e) => RunResult.Error(path, new RuntimeException("Can't parse HOCON file", e))

                case Success(inputRawJson) =>
                  val inputJson = RideRunnerInputParser.parseJson(inputRawJson)
                  AppInitializer.setupChain(RideRunnerInputParser.getChainId(inputJson)) // We must setup chain first to parse addresses
                  RunResult.Succeeded(path, run(inputJson))
              }
            else RunResult.Error(path, new RuntimeException("Isn't a HOCON file") with NoStackTrace)
          }.onErrorRecover(RunResult.Error(path, _))
        }
        .foldLeftL(Stats(0, 0, 0)) { case (r, x) =>
          val file = commonPath.relativize(x.path)
          x match {
            case x: RunResult.Succeeded =>
              println(s"[OK] $file:\n${Json.prettyPrint(x.result).split("\n").mkString("  ", "\n  ", "  ")}\n")
              r.copy(succeeded = r.succeeded + 1)
            case _: RunResult.Failed =>
              println(s"[FAILED] $file\b")
              r.copy(failed = r.failed + 1)
            case x: RunResult.Error =>
              log.warn(s"Error during running $file", x.exception)
              System.err.println(s"[ERROR] $file: ${x.exception.getMessage}\b")
              r.copy(error = r.error + 1)
          }
        }
        .tapEval { x => Task(println(s"Total $x")) }
        .runSyncUnsafe()(global, CanBlock.permit)
    }
  }

  private case class Stats(succeeded: Int, failed: Int, error: Int) {
    override def toString: String = s"succeeded: $succeeded, failed: $failed, error: $error"
  }

  private sealed trait RunResult extends Product with Serializable {
    def path: Path
  }

  private object RunResult {
    case class Succeeded(path: Path, result: JsObject)                  extends RunResult
    case class Failed(path: Path, actual: JsObject, expected: JsObject) extends RunResult
    case class Error(path: Path, exception: Throwable)                  extends RunResult
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

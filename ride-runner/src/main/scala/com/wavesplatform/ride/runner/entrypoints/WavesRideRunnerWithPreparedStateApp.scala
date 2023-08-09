package com.wavesplatform.ride.runner.entrypoints

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import com.wavesplatform.Version
import com.wavesplatform.api.http.utils.UtilsEvaluator
import com.wavesplatform.io.PathUtils
import com.wavesplatform.ride.runner.blockchain.ImmutableBlockchain
import com.wavesplatform.ride.runner.input.{RideRunnerInput, RideRunnerInputParser}
import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, GenesisSettings, RewardsSettings}
import monix.eval.Task
import monix.execution.Scheduler.global
import monix.execution.schedulers.CanBlock
import monix.reactive.Observable
import org.slf4j.LoggerFactory
import play.api.libs.json.*
import scopt.{DefaultOParserSetup, OParser, Read}

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
      val log             = LoggerFactory.getLogger("Main")
      val input           = args.input.map(_.toPath.toAbsolutePath) // Otherwise Lightbend Config doesn't resolve correctly relative paths
      lazy val commonPath = input.reduceLeft(PathUtils.commonPath)

      Observable
        .fromIterable(input)
        .mapParallelOrdered(args.threads) { path =>
          val minimizedPath = if (args.minimizePaths) commonPath.relativize(path) else path
          Task(run(args.runMode, path, minimizedPath)).onErrorRecover(RunResult.Error(minimizedPath, _))
        }
        .foldLeftL(Stats(0, 0, 0)) { case (r, x) =>
          args.printMode.printRunResult(x)
          x match {
            case _: RunResult.Succeeded => r.copy(succeeded = r.succeeded + 1)
            case _: RunResult.Failed    => r.copy(failed = r.failed + 1)
            case x: RunResult.Error =>
              log.warn(s"Error during running ${x.path}", x.exception)
              r.copy(error = r.error + 1)
          }
        }
        .tapEval { x =>
          Task {
            if (args.printTotal) args.printMode.printTotal(x)
          }
        }
        .runSyncUnsafe()(global, CanBlock.permit)
    }
  }

  private def run(runMode: RunMode, path: Path, pathForResult: Path): RunResult = {
    if (path.getFileName.toString.endsWith(".conf"))
      Try(ConfigFactory.parseFile(path.toFile).resolve().root().render(ConfigRenderOptions.concise())) match {
        case Failure(e) => RunResult.Error(pathForResult, new RuntimeException("Can't parse HOCON file", e))

        case Success(inputRawJson) =>
          val inputJson = RideRunnerInputParser.parseJson(inputRawJson)
          // TODO concurrency issue, remove this
          AppInitializer.setupChain(RideRunnerInputParser.getChainId(inputJson)) // We must setup chain first to parse addresses
          val input = RideRunnerInputParser.parse(inputJson)
          lazy val actual = {
            val r = run(input)
            if (input.postProcessing.enable) input.postProcessing.method.process(r)
            else r
          }

          runMode match {
            case RunMode.Run => RunResult.Succeeded(pathForResult, actual)
            case _: RunMode.Test =>
              input.test match {
                case None =>
                  RunResult.Error(pathForResult, new RuntimeException("Is not a test: expected to be have 'test' field.") with NoStackTrace)

                case Some(test) =>
                  if (actual == test.expected) RunResult.Succeeded(pathForResult, actual)
                  else RunResult.Failed(pathForResult, actual, test.expected)
              }
          }

      }
    else RunResult.Error(pathForResult, new RuntimeException("Isn't a HOCON file") with NoStackTrace)
  }

  def run(input: RideRunnerInput): JsObject = {
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

  private implicit val printModeRead: Read[PrintMode] = Read.stringRead.map {
    case "text" => PrintMode.Text
    case "json" => PrintMode.Json
    case x      => throw new IllegalArgumentException(s"Unknown print mode: $x")
  }

  private val commandParser = {
    import scopt.OParser

    val builder = OParser.builder[Args]
    import builder.*

    OParser.sequence(
      head("RIDE script runner", Version.VersionString),
      opt[Unit]('v', "verbose")
        .text("Print logs")
        .action((_, c) => c.copy(verbose = true)),
      opt[Int]('t', "threads")
        .optional()
        .text("A number of parallel threads to process scripts. A number of available cores by default.")
        .action((x, c) => c.copy(threads = x)),
      opt[PrintMode]('p', "printMode")
        .optional()
        .text("Print mode: 'text' or 'json'. 'text' by default.")
        .action((x, c) => c.copy(printMode = x)),
      opt[Unit]("printTotal")
        .optional()
        .text("Print statistics. Not printing by default.")
        .action((_, c) => c.copy(printTotal = true)),
      opt[Unit]("doNotMinimizePaths")
        .optional()
        .text(
          "By default this tool finds a common part of paths of specified files and relativize all paths against it " +
            "for security reasons. Use this option to disable this behavior."
        )
        .action((_, c) => c.copy(minimizePaths = false)),
      help("help").hidden(),
      arg[File]("<file>...")
        .unbounded()
        .required()
        .text("Path to HOCON (conf) file (or files) with prepared state and run arguments.")
        .action((x, c) => c.copy(input = x.getAbsoluteFile :: c.input)),
      cmd("run")
        .text("  Runs scripts and prints results to STDOUT. The default run mode.")
        .action((_, c) => c.copy(runMode = RunMode.Run)),
      cmd("test")
        .text("  Expects that provided files are tests. Runs tests and prints results to STDOUT.")
        .action((_, c) => c.copy(runMode = RunMode.Test()))
        .children(
          opt[File]("junit")
            .text("Write a JUnit report to this file. An existed file will be overwritten.")
            .action((x, c) => c.copy(runMode = RunMode.Test(Some(x))))
        )
    )
  }

  private final case class Args(
      input: List[File] = Nil,
      threads: Int = Runtime.getRuntime.availableProcessors(),
      runMode: RunMode = RunMode.Run,
      printMode: PrintMode = PrintMode.Text,
      minimizePaths: Boolean = true,
      printTotal: Boolean = false,
      verbose: Boolean = false
  )

  private sealed trait RunMode extends Product with Serializable
  private object RunMode {
    case object Run                             extends RunMode
    case class Test(junit: Option[File] = None) extends RunMode
  }

  private sealed trait PrintMode {
    def asPrinted(x: Printable): String

    def printRunResult(x: RunResult): Unit = x match {
      case x: RunResult.Error => System.err.println(asPrinted(x))
      case _                  => println(asPrinted(x))
    }

    def printTotal(x: Stats): Unit = println(asPrinted(x))
  }

  private object PrintMode {
    case object Text extends PrintMode {
      override def asPrinted(x: Printable): String = x.asString
    }

    case object Json extends PrintMode {
      override def asPrinted(x: Printable): String = x.asJson.toString()
    }
  }

  private case class Stats(succeeded: Int, failed: Int, error: Int) extends Printable {
    override def asJson: JsObject = Json.obj(
      "succeeded" -> succeeded,
      "failed"    -> failed,
      "error"     -> error
    )

    override def asString: String = s"Total succeeded: $succeeded, failed: $failed, error: $error"
  }

  // TODO move path outside?
  private sealed trait RunResult extends Printable with Product with Serializable {
    def path: Path
  }

  private object RunResult {
    private def padLeft(x: String, padding: String): String = x.split('\n').mkString(padding, s"\n$padding", padding)

    case class Succeeded(path: Path, result: JsValue) extends RunResult {
      override def asJson: JsObject = Json.obj(
        "path"   -> path.toString,
        "status" -> "ok",
        "result" -> result
      )

      override def asString: String = s"[OK] $path:\n${padLeft(Json.prettyPrint(result), "  ")}"
    }

    case class Failed(path: Path, actual: JsValue, expected: JsValue) extends RunResult {
      override def asJson: JsObject = Json.obj(
        "path"     -> path.toString,
        "status"   -> "failed",
        "result"   -> actual,
        "expected" -> expected
      )

      override def asString: String =
        s"[FAILED] $path\n" +
          s"  expected:\n${padLeft(Json.prettyPrint(expected), "    ")}\n" +
          s"  actual:\n${padLeft(Json.prettyPrint(actual), "    ")}"
    }

    case class Error(path: Path, exception: Throwable) extends RunResult {
      override def asJson: JsObject = Json.obj(
        "path"    -> path.toString,
        "status"  -> "error",
        "result"  -> JsNull,
        "message" -> exception.getMessage
      )

      override def asString: String = s"[ERROR] $path: ${exception.getMessage}"
    }
  }

  private sealed trait Printable {
    def asJson: JsObject
    def asString: String
  }
}

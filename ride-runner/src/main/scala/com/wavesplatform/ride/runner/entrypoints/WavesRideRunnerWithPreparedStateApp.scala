package com.wavesplatform.ride.runner.entrypoints

import cats.syntax.option.*
import com.typesafe.config.ConfigFactory
import com.wavesplatform.Version
import com.wavesplatform.api.http.utils.UtilsEvaluator
import com.wavesplatform.ride.runner.RideTestSuite
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
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration.{DurationInt, DurationLong, FiniteDuration}
import scala.jdk.CollectionConverters.EnumerationHasAsScala
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success, Try, Using}
import scala.xml.{Elem, PCData}

object WavesRideRunnerWithPreparedStateApp {
  def main(args: Array[String]): Unit = {
    val setup = new DefaultOParserSetup {
      override val showUsageOnError = Some(true)
    }

    OParser.parse(commandParser, args, Args(), setup).foreach { args =>
      System.setProperty("logback.stdout.level", if (args.verbose) "TRACE" else "OFF")
      val log   = LoggerFactory.getLogger("Main")
      val input = args.input.map(_.toPath)

      lazy val junitReport = {
        val junitFile = args.runMode match {
          case RunMode.Run         => none
          case RunMode.Test(junit) => junit
        }
        junitFile.fold(new JUnitReport())(new FileJUnitReport(_))
      }

      val stats = Observable
        .fromIterable(input)
        .mapParallelOrdered(args.threads) { path =>
          val start = System.nanoTime()
          Task(run(args.runMode, path))
            .onErrorRecover(RunResultStatus.Error(_))
            .map(RunResult(path, (System.nanoTime() - start).nanos, _))
        }
        .foldLeftL(Stats(0, 0, 0)) { case (r, x) =>
          x.status match {
            case s: RunResultStatus.Error => log.warn(s"Error during running ${x.path}", s.exception)
            case _                        =>
          }
          args.printMode.printRunResult(x, args.colorizer)
          junitReport.addTestCase(x)
          r.updated(x)
        }
        .tapEval { x => Task { if (args.printTotal) args.printMode.printTotal(x, args.colorizer) } }
        .runSyncUnsafe()(global, CanBlock.permit)

      junitReport.writeReport()
      System.exit(stats.appStatus)
    }
  }

  private def run(runMode: RunMode, path: Path): RunResultStatus =
    if (path.getFileName.toString.endsWith(".conf")) {
      // toAbsolutePath is required, otherwise Lightbend Config incorrectly resolve relative paths
      Try(RideRunnerInputParser.prepare(ConfigFactory.parseFile(path.toAbsolutePath.toFile))) match {
        case Failure(e) => RunResultStatus.Error(new RuntimeException(s"Can't parse HOCON file: ${e.getMessage}", e))
        case Success(inputConfig) =>
          AppInitializer.setupChain(RideRunnerInputParser.getChainId(inputConfig)) // We must setup chain first to parse addresses
          val input = RideRunnerInputParser.from(inputConfig)
          lazy val actual = {
            val r = run(input)
            input.postProcessing.foldLeft[JsValue](r) { case (r, x) => x.process(r) }
          }

          runMode match {
            case RunMode.Run => RunResultStatus.Succeeded(actual)
            case _: RunMode.Test =>
              input.test match {
                case None => RunResultStatus.Error(new RuntimeException("Is not a test: expected to have the 'test' field.") with NoStackTrace)
                case Some(test) =>
                  if (actual == test.expected) RunResultStatus.Succeeded(actual)
                  else RunResultStatus.Failed(actual, test.expected)
              }
          }

      }
    } else RunResultStatus.Error(new RuntimeException("Isn't a HOCON file") with NoStackTrace)

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
      opt[Unit]('M', "monochrome")
        .optional()
        .text("Don't colorize. Colorize by default.")
        .action((_, c) => c.copy(colorizer = MonochromeColorizer)),
      help("help").hidden(),
      arg[File]("<file>...")
        .unbounded()
        .required()
        .text("Path to HOCON (conf) file (or files) with prepared state and run arguments.")
        .action((x, c) => c.copy(input = x :: c.input)),
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
      printTotal: Boolean = false,
      colorizer: Colorizer = new RealColorizer,
      verbose: Boolean = false
  )

  private sealed trait RunMode extends Product with Serializable
  private object RunMode {
    case object Run                             extends RunMode
    case class Test(junit: Option[File] = None) extends RunMode
  }

  private sealed trait PrintMode {
    def asPrinted(x: Printable, colorizer: Colorizer): String

    def printRunResult(x: RunResult, colorizer: Colorizer): Unit = x.status match {
      case _: RunResultStatus.Error => System.err.println(asPrinted(x, colorizer))
      case _                        => println(asPrinted(x, colorizer))
    }

    def printTotal(x: Stats, colorizer: Colorizer): Unit = println(asPrinted(x, colorizer))
  }

  private object PrintMode {
    case object Text extends PrintMode {
      override def asPrinted(x: Printable, colorizer: Colorizer): String = x.asString(colorizer)
    }

    case object Json extends PrintMode {
      override def asPrinted(x: Printable, colorizer: Colorizer): String = x.asJson.toString()
    }
  }

  private sealed class JUnitReport {
    def addTestCase(result: RunResult): Unit = {}
    def writeReport(): Unit                  = {}
  }

  // Information about a file format: https://github.com/testmoapp/junitxml
  private class FileJUnitReport(output: File) extends JUnitReport {
    private val colorizer = MonochromeColorizer
    private val testCases = new ConcurrentHashMap[Path, RunResult]

    override def addTestCase(result: RunResult): Unit = testCases.put(result.path, result)

    override def writeReport(): Unit = {
      val rootTestSuite      = RideTestSuite.minimize(RideTestSuite.getTestSuites(testCases.keys().asScala.toList))
      val (inner, totalTime) = render(rootTestSuite)

      val xml: Elem =
        <testsuites time={formatSpentTime(totalTime)} timestamp={Instant.now.toString}>
          {inner}
        </testsuites>

      val pp         = new scala.xml.PrettyPrinter(120, 2, minimizeEmpty = true)
      val outputPath = output.toPath.toAbsolutePath
      Files.createDirectories(outputPath.getParent)
      Files.write(
        outputPath,
        pp.format(xml).getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
    }

    private def render(testSuite: RideTestSuite): (Elem, FiniteDuration) = {
      val (childTestSuites, testSuitesTotalTime) = sumTime(testSuite.testSuites.view.map(render))
      val (childTestCases, testCasesTotalTime)   = sumTime(testSuite.testCases.view.map(path => render(testCases.get(path))))
      val testSuiteName = Option(testSuite.path.getFileName).map(_.toString) match {
        case Some("") => "."
        case Some(x)  => x
        case None     => "???"
      }

      val xml =
        <testsuite name={testSuiteName} time={formatSpentTime(testSuitesTotalTime + testCasesTotalTime)}>
          {childTestSuites}
          {childTestCases}
        </testsuite>
      (xml, testSuitesTotalTime + testCasesTotalTime)
    }

    private def render(testCase: RunResult): (Elem, FiniteDuration) = {
      val content = testCase.status match {
        case s: RunResultStatus.Succeeded =>
          <system-out>{PCData(s.asString(colorizer))}</system-out>

        case s: RunResultStatus.Failed =>
          <failure message="Expected value did not match" type="AssertionError">
            {PCData(s.asString(colorizer))}
          </failure>

        case s: RunResultStatus.Error =>
          val errorMessage = Option(s.exception.getMessage).fold("Undefined") { e =>
            if (e.length > 120) s"${e.take(120)}..."
            else e
          }
          <error message={errorMessage} type={s.exception.getClass.getSimpleName}>
            {PCData(getStackTrace(s.exception))}
          </error>
      }

      // "name" is required for JUnit report parsers. It can be empty.
      val xml =
        <testcase name="" file={testCase.path.toString} time={formatSpentTime(testCase.duration)}>
          {content}
        </testcase>

      (xml, testCase.duration)
    }

    private def sumTime(xs: Iterable[(Elem, FiniteDuration)]): (List[Elem], FiniteDuration) =
      xs.foldLeft((List.empty[Elem], 0.seconds)) { case ((elems, totalTime), (elem, time)) =>
        (elem :: elems, time + totalTime)
      }

    private def formatSpentTime(x: FiniteDuration): String = (x.toMillis / 1000.0d).toString

    private def getStackTrace(ex: Throwable): String = Using
      .Manager { use =>
        val sw = use(new java.io.StringWriter)
        val pw = use(new java.io.PrintWriter(sw))
        ex.printStackTrace(pw)
        sw.toString
      }
      .getOrElse("Can't get a stack trace")

  }

  private case class Stats(succeeded: Int, failed: Int, error: Int) extends Printable {
    def appStatus: Int = if (failed == 0 && error == 0) 0 else 1

    override def asJson: JsObject = Json.obj(
      "succeeded" -> succeeded,
      "failed"    -> failed,
      "error"     -> error
    )

    def updated(x: RunResult): Stats = x.status match {
      case _: RunResultStatus.Succeeded => copy(succeeded = succeeded + 1)
      case _: RunResultStatus.Failed    => copy(failed = failed + 1)
      case _: RunResultStatus.Error     => copy(error = error + 1)
    }

    override def asString(colorizer: Colorizer): String = s"Total succeeded: $succeeded, failed: $failed, error: $error"
  }

  private case class RunResult(path: Path, duration: FiniteDuration, status: RunResultStatus) extends Printable {
    override def asJson: JsObject = Json.obj(
      "path"     -> path.toString,
      "timeInMs" -> duration.toMillis,
      "status"   -> status.asJson
    )

    override def asString(colorizer: Colorizer): String =
      s"[${duration.toCoarsest}] ${colorizer.colorize(path.toString, Colorizer.Color.Blue)} ${status.asString(colorizer)}"
  }

  private sealed trait RunResultStatus extends Printable with Product with Serializable

  private object RunResultStatus {
    private def padLeft(x: String, padding: String): String = x.split('\n').mkString(padding, s"\n$padding", padding)

    case class Succeeded(result: JsValue) extends RunResultStatus {
      override def asJson: JsObject = Json.obj(
        "status" -> "succeeded",
        "result" -> result
      )

      override def asString(colorizer: Colorizer): String =
        s"${colorizer.colorize("succeeded", Colorizer.Color.Green)}:\n" +
          s"${padLeft(Json.prettyPrint(result), "  ")}"
    }

    case class Failed(actual: JsValue, expected: JsValue) extends RunResultStatus {
      override def asJson: JsObject = Json.obj(
        "status"   -> "failed",
        "result"   -> actual,
        "expected" -> expected
      )

      override def asString(colorizer: Colorizer): String = s"${colorizer.colorize("failed", Colorizer.Color.Red)}:\n" +
        s"  expected:\n${padLeft(Json.prettyPrint(expected), "    ")}\n" +
        s"  actual:\n${padLeft(Json.prettyPrint(actual), "    ")}"
    }

    case class Error(exception: Throwable) extends RunResultStatus {
      override def asJson: JsObject = Json.obj(
        "status"  -> "error",
        "result"  -> JsNull,
        "message" -> exception.getMessage
      )

      override def asString(colorizer: Colorizer): String = s"${colorizer.colorize("error", Colorizer.Color.Red)}: ${exception.getMessage}"
    }
  }

  private sealed trait Printable {
    def asJson: JsObject
    def asString(colorizer: Colorizer): String
  }

  private sealed trait Colorizer {
    def colorize(text: String, color: Colorizer.Color): String
  }

  private object Colorizer {
    sealed abstract class Color(val number: Int)
    object Color {
      case object Red   extends Color(31)
      case object Green extends Color(32)
      case object Blue  extends Color(34)
    }
  }

  private object MonochromeColorizer extends Colorizer {
    def colorize(text: String, color: Colorizer.Color): String = text
  }

  private class RealColorizer extends Colorizer {
    def colorize(text: String, color: Colorizer.Color): String = s"\u001b[${color.number}m$text\u001b[0m"
  }
}

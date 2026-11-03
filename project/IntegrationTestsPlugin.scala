import com.github.dockerjava.core.{DefaultDockerClientConfig, DockerClientImpl}
import com.github.dockerjava.httpclient5.ApacheDockerHttpClient
import sbt.*
import sbt.Keys.*
import sbt.Tests.Group

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.control.NonFatal

// Separate projects for integration tests because of IDEA: https://youtrack.jetbrains.com/issue/SCL-14363#focus=streamItem-27-3061842.0-0
object IntegrationTestsPlugin extends AutoPlugin {

  object autoImport extends ItKeys
  import autoImport.*

  private val LoadTestTag = "com.wavesplatform.it.LoadTest"

  /** Runs exactly the named suites through `testOnly`, failing loudly rather than reporting a green empty run. */
  private def runSuites(select: ((Seq[String], Seq[String])) => Seq[String], emptyWhy: String, tagged: Boolean) =
    Def.taskDyn {
      // testOnly still inherits the `-l` built from SCALATEST_EXCLUDE_TAGS, so excluding the very tag loadTests
      // selects on would filter every test out of the suites it forked and still exit 0.
      val selfDefeating =
        tagged && sys.env.get("SCALATEST_EXCLUDE_TAGS").exists(_.split("\\s+").contains(LoadTestTag))
      select(loadTestNames.value) match {
        case Nil => Def.task[Unit](sys.error(s"Nothing to run: $emptyWhy"))
        case _ if selfDefeating =>
          Def.task[Unit](sys.error(s"SCALATEST_EXCLUDE_TAGS excludes $LoadTestTag, so every selected test would be filtered out"))
        case selected => (Test / testOnly).toTask(selected.mkString(" ", " ", "")).map(_ => ())
      }
    }

  override def projectSettings: Seq[Def.Setting[_]] =
    inConfig(Test)(
      Seq(
        logDirectory := {
          val runId = Option(System.getenv("RUN_ID")).getOrElse {
            val formatter = DateTimeFormatter.ofPattern("MM-dd--HH_mm_ss")
            formatter.format(LocalDateTime.now()) // git branch?
          }
          val r = target.value / "logs" / runId
          IO.createDirectory(r)
          r
        },
        // Example: SCALATEST_EXCLUDE_TAGS="package1.Tag1 package2.Tag2 package3.Tag3"
        testOptions += {
          val excludeTags = sys.env.get("SCALATEST_EXCLUDE_TAGS").fold(Seq.empty[String])(Seq("-l", _))
          /* http://www.scalatest.org/user_guide/using_the_runner
           * f - select the file reporter with output directory
           * F - show full stack traces
           * W - without color
           */
          val args = Seq("-fFW", (logDirectory.value / "summary.log").toString) ++ excludeTags
          Tests.Argument(TestFrameworks.ScalaTest, args: _*)
        },
        // Both halves of the load-test split derive from this one name, so neither can drift: a renamed annotation is
        // an error instead of an `-n`/`-l` tag argument that silently selects nothing (and exits 0).
        loadTestNames := {
          val loader = testLoader.value
          val loadTest = scala.util
            .Try(Class.forName(LoadTestTag, false, loader))
            .getOrElse(sys.error(s"$LoadTestTag not found - has the annotation been moved or renamed?"))
            .asSubclass(classOf[java.lang.annotation.Annotation])
          definedTests.value.map(_.name).partition(Class.forName(_, false, loader).isAnnotationPresent(loadTest))
        },
        loadTests         := runSuites(_._1, s"no suite carries @$LoadTestTag", tagged = true).value,
        integrationTests  := runSuites(_._2, "no untagged suites found", tagged = false).value,
        parallelExecution := true,
        // sbt tags every forked group with these too, so the default (Tags.Test, 1) puts suites under build.sbt's
        // per-processor Tags.Test cap (4 on a GitHub runner) and the flag could only lower concurrency, never raise it.
        // Cleared, the ForkedTestGroup limit (-Dwaves.it.max-parallel-suites) is the only one that applies.
        testOnly / tags := Seq.empty,
        test / tags     := Seq.empty,
        testGrouping := {
          // ffs, sbt!
          // https://github.com/sbt/sbt/issues/3266
          val javaHomeValue     = javaHome.value
          val logDirectoryValue = logDirectory.value
          val envVarsValue      = envVars.value
          val javaOptionsValue  = javaOptions.value

          for {
            group <- testGrouping.value
            suite <- group.tests
          } yield Group(
            suite.name,
            Seq(suite),
            Tests.SubProcess(
              ForkOptions(
                javaHome = javaHomeValue,
                outputStrategy = outputStrategy.value,
                bootJars = Vector.empty[java.io.File],
                workingDirectory = Option(baseDirectory.value),
                runJVMOptions = Vector(
                  "-Dwaves.it.logging.appender=FILE",
                  s"-Dwaves.it.logging.dir=${logDirectoryValue / suite.name.replaceAll("""(\w)\w*\.""", "$1.")}" // foo.bar.Baz -> f.b.Baz
                ) ++ javaOptionsValue,
                connectInput = false,
                envVars = envVarsValue
              )
            )
          )
        }
      )
    ) ++ inScope(Global)(
      Seq(
        maxParallelSuites := Option(Integer.getInteger("waves.it.max-parallel-suites"))
          .getOrElse[Integer] {
            try {
              val config       = DefaultDockerClientConfig.createDefaultConfigBuilder().build()
              val httpClient   = new ApacheDockerHttpClient.Builder().dockerHost(config.getDockerHost).build()
              val dockerClient = DockerClientImpl.getInstance(config, httpClient)
              try {
                val dockerCpu = dockerClient.infoCmd().exec().getNCPU
                sLog.value.info(s"Docker CPU count: $dockerCpu")
                dockerCpu * 2
              } finally {
                httpClient.close()
                dockerClient.close()
              }
            } catch {
              case NonFatal(e) =>
                sLog.value.warn(s"Could not connect to Docker, is the daemon running? ${e.getMessage}")
                sLog.value.info(s"System CPU count: ${EvaluateTask.SystemProcessors}")
                EvaluateTask.SystemProcessors
            }
          }
        // concurrentRestrictions lives in build.sbt: a second Global assignment replaces, not adds.
      )
    )

}

trait ItKeys {
  val logDirectory      = taskKey[File]("The directory where logs of integration tests are written")
  val maxParallelSuites = settingKey[Int]("Number of test suites to run in parallel")
  val loadTestNames     = taskKey[(Seq[String], Seq[String])]("Suite names, split into @LoadTest-annotated and the rest")
  val loadTests         = taskKey[Unit]("Runs only the @LoadTest-annotated suites")
  val integrationTests  = taskKey[Unit]("Runs every suite except the @LoadTest-annotated ones")
}

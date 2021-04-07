import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import com.spotify.docker.client.DefaultDockerClient
import sbt.Keys._
import sbt.Tests.Group
import sbt._

import scala.util.control.NonFatal

// Separate projects for integration tests because of IDEA: https://youtrack.jetbrains.com/issue/SCL-14363#focus=streamItem-27-3061842.0-0
object IntegrationTestsPlugin extends AutoPlugin {

  object autoImport extends ItKeys
  import autoImport._

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
        parallelExecution := true,
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
              val docker = DefaultDockerClient.fromEnv().build()
              try {
                val dockerCpu: Int = docker.info().cpus()
                sLog.value.info(s"Docker CPU count: $dockerCpu")
                (dockerCpu * 1.5).toInt
              } finally docker.close()
            } catch {
              case NonFatal(_) =>
                sLog.value.warn(s"Could not connect to Docker, is the daemon running?")
                sLog.value.info(s"System CPU count: ${EvaluateTask.SystemProcessors}")
                EvaluateTask.SystemProcessors
            }
          },
        concurrentRestrictions := Seq(
          Tags.limit(Tags.ForkedTestGroup, maxParallelSuites.value)
        )
      )
    )

}

trait ItKeys {
  val logDirectory      = taskKey[File]("The directory where logs of integration tests are written")
  val maxParallelSuites = settingKey[Int]("Number of test suites to run in parallel")
}

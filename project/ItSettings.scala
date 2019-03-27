import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import sbt.Keys._
import sbt.Tests.Group
import sbt._

// Separate projects for integration tests because of IDEA: https://youtrack.jetbrains.com/issue/SCL-14363#focus=streamItem-27-3061842.0-0
object ItSettings {
  val logDirectory = taskKey[File]("Directory with integration test logs")

  def settings: Seq[Def.Setting[_]] =
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
        testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-fFW", (logDirectory.value / "summary.log").toString),
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
          } yield
            Group(
              suite.name,
              Seq(suite),
              Tests.SubProcess(
                ForkOptions(
                  javaHome = javaHomeValue,
                  outputStrategy = outputStrategy.value,
                  bootJars = Vector.empty[java.io.File],
                  workingDirectory = Option(baseDirectory.value),
                  runJVMOptions = Vector(
                    "-XX:+IgnoreUnrecognizedVMOptions",
                    "--add-modules=java.xml.bind",
                    "-Dwaves.it.logging.appender=FILE",
                    s"-Dwaves.it.logging.dir=${logDirectoryValue / suite.name.replaceAll("""(\w)\w*\.""", "$1.")}"
                  ) ++ javaOptionsValue,
                  connectInput = false,
                  envVars = envVarsValue
                ))
            )
        }
      ))
}

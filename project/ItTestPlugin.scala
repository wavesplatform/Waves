import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import sbt.Keys._
import sbt.Tests.Group
import sbt._

// Separate projects for integration tests because of IDEA: https://youtrack.jetbrains.com/issue/SCL-14363#focus=streamItem-27-3061842.0-0
object ItTestPlugin extends AutoPlugin {

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
                    "-Dwaves.it.logging.appender=FILE",
                    s"-Dwaves.it.logging.dir=${logDirectoryValue / suite.name.replaceAll("""(\w)\w*\.""", "$1.")}" // foo.bar.Baz -> f.b.Baz
                  ) ++ javaOptionsValue ++ ModernJavaSettings.options,
                  connectInput = false,
                  envVars = envVarsValue
                ))
            )
        }
      ))
}

trait ItKeys {
  val logDirectory = taskKey[File]("The directory where logs of integration tests are written")
}

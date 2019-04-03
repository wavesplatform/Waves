import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import sbt.Tests.Group

concurrentRestrictions in Global := {
  val threadNumber = Option(System.getenv("SBT_THREAD_NUMBER")).fold(1)(_.toInt)
  Seq(
    Tags.limit(Tags.CPU, threadNumber),
    Tags.limit(Tags.Network, threadNumber),
    Tags.limit(Tags.Test, threadNumber),
    Tags.limitAll(threadNumber)
  )
}
enablePlugins(sbtdocker.DockerPlugin)

inTask(docker)(
  Seq(
    dockerfile := {
      val configTemplate = (Compile / resourceDirectory).value / "template.conf"
      val startWaves     = sourceDirectory.value / "container" / "start-waves.sh"

      val withAspectJ     = Option(System.getenv("WITH_ASPECTJ")).fold(false)(_.toBoolean)
      val aspectjAgentUrl = "http://search.maven.org/remotecontent?filepath=org/aspectj/aspectjweaver/1.9.1/aspectjweaver-1.9.1.jar"
      val yourKitArchive  = "YourKit-JavaProfiler-2019.1-docker.zip"

      new Dockerfile {
        from("anapsix/alpine-java:8_server-jre")
        runRaw("mkdir -p /opt/waves")

        // Install YourKit
        runRaw(s"""apk update && \\
                  |apk add --no-cache openssl ca-certificates && \\
                  |wget --quiet https://www.yourkit.com/download/docker/$yourKitArchive -P /tmp/ && \\
                  |unzip /tmp/$yourKitArchive -d /usr/local && \\
                  |rm -f /tmp/$yourKitArchive""".stripMargin)

        if (withAspectJ) run("wget", "--quiet", aspectjAgentUrl, "-O", "/opt/waves/aspectjweaver.jar")

        add((assembly in LocalProject("node")).value, "/opt/waves/waves.jar")
        add(Seq(configTemplate, startWaves), "/opt/waves/")
        runShell("chmod", "+x", "/opt/waves/start-waves.sh")
        entryPoint("/opt/waves/start-waves.sh")
        expose(10001)
      }
    },
    buildOptions := BuildOptions(removeIntermediateContainers = BuildOptions.Remove.OnSuccess)
  ))

libraryDependencies ++= Dependencies.testKit ++ Dependencies.itKit

val logDirectory = Def.task {
  val runId = Option(System.getenv("RUN_ID")).getOrElse {
    val formatter = DateTimeFormatter.ofPattern("MM-dd--HH_mm_ss")
    s"local-${formatter.format(LocalDateTime.now())}"
  }
  val r = target.value / "logs" / runId
  IO.createDirectory(r)
  r
}

lazy val itTestsCommonSettings: Seq[Def.Setting[_]] = Seq(
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
)

inConfig(Test) {
  val commonFlags = "-XX:+UnlockExperimentalVMOptions -XX:+UseCGroupMemoryLimitForHeap"
  Seq(
    test := (test dependsOn docker).value,
    envVars in test += "CONTAINER_JAVA_OPTS"     -> s"-Xmx1500m $commonFlags",
    envVars in testOnly += "CONTAINER_JAVA_OPTS" -> s"-Xmx512m $commonFlags"
  ) ++ inTask(test)(itTestsCommonSettings) ++ inTask(testOnly)(itTestsCommonSettings)
}

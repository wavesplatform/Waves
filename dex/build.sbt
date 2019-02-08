import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import Dependencies.akkaModule
import sbt.Tests.Group
import sbtdocker.DockerPlugin
import DockerSettings.additionalFiles

enablePlugins(ExtensionPackaging)

//concurrentRestrictions in Global := {
//  val threadNumber = Option(System.getenv("SBT_THREAD_NUMBER")).fold(1)(_.toInt)
//  Seq(
//    Tags.limit(Tags.CPU, threadNumber),
//    Tags.limit(Tags.Network, threadNumber),
//    Tags.limit(Tags.Test, threadNumber),
//    Tags.limitAll(threadNumber)
//  )
//}

Test / fork := true

libraryDependencies ++= Seq(
  "com.typesafe.akka"   %% "akka-stream-kafka"         % "1.0",
  "com.github.dnvriend" %% "akka-persistence-inmemory" % "2.5.15.1" % "test",
  ("org.iq80.leveldb" % "leveldb" % "0.9" % "test").exclude("com.google.guava", "guava")
) ++ Seq(
  akkaModule("testkit"),
  akkaModule("persistence-tck")
).map(_ % "test") ++ Dependencies.test // ++ Dependencies.itTest

//val logDirectory = Def.task {
//  val runId = Option(System.getenv("RUN_ID")).getOrElse {
//    val formatter = DateTimeFormatter.ofPattern("MM-dd--HH_mm_ss")
//    s"local-${formatter.format(LocalDateTime.now())}"
//  }
//  val r = target.value / "logs" / runId
//  IO.createDirectory(r)
//  r
//}

//lazy val itTestsCommonSettings: Seq[Def.Setting[_]] = Seq(
//  testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-fFW", (logDirectory.value / "summary.log").toString),
//  testGrouping := {
//    // ffs, sbt!
//    // https://github.com/sbt/sbt/issues/3266
//    val javaHomeValue     = javaHome.value
//    val logDirectoryValue = logDirectory.value
//    val envVarsValue      = envVars.value
//    val javaOptionsValue  = javaOptions.value
//
//    for {
//      group <- testGrouping.value
//      suite <- group.tests
//    } yield
//      Group(
//        suite.name,
//        Seq(suite),
//        Tests.SubProcess(
//          ForkOptions(
//            javaHome = javaHomeValue,
//            outputStrategy = outputStrategy.value,
//            bootJars = Vector.empty[java.io.File],
//            workingDirectory = Option(baseDirectory.value),
//            runJVMOptions = Vector(
//              "-XX:+IgnoreUnrecognizedVMOptions",
//              "--add-modules=java.xml.bind",
//              "-Dwaves.it.logging.appender=FILE",
//              s"-Dwaves.it.logging.dir=${logDirectoryValue / suite.name.replaceAll("""(\w)\w*\.""", "$1.")}"
//            ) ++ javaOptionsValue,
//            connectInput = false,
//            envVars = envVarsValue
//          ))
//      )
//  }
//)

//docker / additionalFiles += (Universal / stage).value
//inConfig(IntegrationTest) {
//  val commonFlags = "-XX:+UnlockExperimentalVMOptions -XX:+UseCGroupMemoryLimitForHeap"
//  Seq(
//    test := (test dependsOn docker).value,
//    envVars in test += "CONTAINER_JAVA_OPTS"     -> s"-Xmx1500m $commonFlags",
//    envVars in testOnly += "CONTAINER_JAVA_OPTS" -> s"-Xmx512m $commonFlags"
//  ) ++ inTask(test)(itTestsCommonSettings) ++ inTask(testOnly)(itTestsCommonSettings)
//}

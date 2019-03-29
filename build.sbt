// IDEA notes
// * May require to run from SBT: node / Compile / managedSources
// * If you choose "use sbt" for running tests, then debug will not work because of fork: https://www.jetbrains.com/help/idea/run-debug-and-test-scala.html#28575304
//   Also the logs will be written to the working directory, because we can't use SBT javaOptions.
//
// * To work with scratches, make sure:
//   1. You've selected the appropriate project
//   2. You've checked "Make project before run"

import sbt.Keys._
import sbt._
import sbtcrossproject.CrossPlugin.autoImport.crossProject

lazy val common = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .disablePlugins(ProtocPlugin)
  .settings(
    libraryDependencies += Dependencies.scalaTest,
    coverageExcludedPackages := ""
  )

lazy val commonJS  = common.js
lazy val commonJVM = common.jvm

lazy val lang =
  crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .disablePlugins(ProtocPlugin)
    .dependsOn(common % "compile;test->test")
    .settings(
      version := "1.0.0",
      coverageExcludedPackages := ".*",
      test in assembly := {},
      libraryDependencies ++= Dependencies.lang.value ++ Dependencies.test,
      resolvers += Resolver.bintrayIvyRepo("portable-scala", "sbt-plugins"),
      resolvers += Resolver.sbtPluginRepo("releases")
      // Compile / scalafmt / sourceDirectories += file("shared").getAbsoluteFile / "src" / "main" / "scala" // doesn't work
    )

lazy val langJS  = lang.js
lazy val langJVM = lang.jvm

lazy val node = project
  .dependsOn(
    commonJVM % "compile;test->test",
    langJVM   % "compile;test->test"
  )

lazy val `node-it` = project.dependsOn(node)

lazy val `node-generator` = project.dependsOn(node, `node-it` % "compile->test")

lazy val benchmark = project
  .dependsOn(
    node    % "compile;test->test",
    langJVM % "compile;test->test"
  )

lazy val dex = project.dependsOn(node % "compile;test->test;runtime->provided")

lazy val `dex-it` = project
  .dependsOn(
    dex,
    `node-it` % "compile;test->test"
  )

lazy val `dex-generator` = project.dependsOn(dex, `dex-it` % "compile->test")

lazy val checkPRRaw = taskKey[Unit]("Build a project and run unit tests")

lazy val root = (project in file("."))
  .settings(
    checkPRRaw := {
      try {
        clean.value // Hack to run clean before all tasks
      } finally {
        test.all(ScopeFilter(inProjects(commonJVM, langJVM, node, dex), inConfigurations(Test))).value
        (commonJS / Compile / fastOptJS).value
        (langJS / Compile / fastOptJS).value
        compile.all(ScopeFilter(inProjects(`node-generator`, benchmark, `dex-generator`), inConfigurations(Test))).value
      }
    }
  )
  .aggregate(
    commonJS,
    commonJVM,
    langJS,
    langJVM,
    node,
    `node-it`,
    `node-generator`,
    benchmark,
    dex,
    `dex-it`,
    `dex-generator`
  )

inScope(Global)(
  Seq(
    scalaVersion := "2.12.8",
    organization := "com.wavesplatform",
    organizationName := "Waves Platform",
    organizationHomepage := Some(url("https://wavesplatform.com")),
    scmInfo := Some(ScmInfo(url("https://github.com/wavesplatform/Waves"), "git@github.com:wavesplatform/Waves.git", None)),
    licenses := Seq(("MIT", url("https://github.com/wavesplatform/Waves/blob/master/LICENSE"))),
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-Ywarn-unused:-implicits",
      "-Xlint",
      "-Ywarn-unused-import",
      "-Ypartial-unification"
    ),
    crossPaths := false,
    scalafmtOnCompile := true,
    dependencyOverrides ++= Dependencies.enforcedVersions.value,
    cancelable := true,
    logBuffered := false,
    coverageExcludedPackages := ".*",
    parallelExecution := false,
    testListeners := Seq.empty, // Fix for doubled test reports
    testOptions += Tests.Argument("-oIDOF", "-u", "target/test-reports"),
    testOptions += Tests.Setup(_ => sys.props("sbt-testing") = "true"),
    concurrentRestrictions := {
      val threadNumber = Option(System.getenv("SBT_THREAD_NUMBER")).fold(1)(_.toInt)
      Seq(Tags.limit(Tags.ForkedTestGroup, threadNumber))
    }
  ))

// ThisBuild options
git.useGitDescribe := true
git.uncommittedSignifier := Some("DIRTY")

// TODO: https://stackoverflow.com/a/14274715
addCommandAlias(
  "checkPR",
  // set scalacOptions in ThisBuild ++= Seq("-Xfatal-warnings");
  """;
    |Global / checkPRRaw;
    |set scalacOptions in ThisBuild -= "-Xfatal-warnings";
  """.stripMargin
)

commands += Command.command("packageAll") { state =>
  "node/clean" :: "node/assembly" :: "node/debian:packageBin" :: state
}

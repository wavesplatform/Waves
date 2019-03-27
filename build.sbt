// IDEA notes
// * Do not name the root directory with the existed project name (node, dex, ...)
// * To run integration tests from IDEA, enable the "sbt" checkbox
// * May require to run from SBT: node / Compile / managedSources

import sbt.Keys._
import sbt._
import sbtcrossproject.CrossPlugin.autoImport.crossProject

lazy val common = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .disablePlugins(ProtocPlugin)
  .settings(
    libraryDependencies += Dependencies.scalaTest,
    //resolvers += Resolver.bintrayIvyRepo("portable-scala", "sbt-plugins"), //
    //resolvers += Resolver.sbtPluginRepo("releases"), //
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
      resolvers += Resolver.sbtPluginRepo("releases"),
      Compile / scalafmt / sourceDirectories += file("shared").getAbsoluteFile / "src" / "main" / "scala" // doesn't work
    )

lazy val langJS  = lang.js
lazy val langJVM = lang.jvm

lazy val node = project
  .dependsOn(
    commonJVM % "compile;test->test",
    langJVM   % "compile;test->test"
  )

lazy val `node-it` = project.dependsOn(node)

lazy val `node-generator` = project.dependsOn(node % "compile->test")

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

lazy val `dex-generator` = project.dependsOn(dex)

lazy val checkPRRaw = taskKey[Unit]("Build a project and run unit tests")

git.useGitDescribe := true
git.uncommittedSignifier := Some("DIRTY")

inScope(Global)(
  Seq(
    scalaVersion := "2.12.8",
    organization := "com.wavesplatform",
    organizationName := "Waves Platform",
    organizationHomepage := Some(url("https://wavesplatform.com")),
    scmInfo := Some(ScmInfo(url("https://github.com/wavesplatform/Waves"), "git@github.com:wavesplatform/Waves.git", None)),
    licenses := Seq(("MIT", url("https://github.com/wavesplatform/Waves/blob/master/LICENSE"))),
    crossPaths := false,
    scalafmtOnCompile := true,
    dependencyOverrides ++= Dependencies.enforcedVersions.value,
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
    cancelable := true,
    logBuffered := false,
    coverageExcludedPackages := ".*",
    checkPRRaw := {
      try {
        clean.all(ScopeFilter(inAnyProject)).value
      } finally {
        test.all(ScopeFilter(inProjects(commonJVM, langJVM, node, dex), inConfigurations(Test))).value
        (commonJS / Compile / fastOptJS).value
        (langJS / Compile / fastOptJS).value
        compile.all(ScopeFilter(inProjects(`node-generator`, benchmark, `dex-generator`), inConfigurations(Test))).value
      }
    },
    logBuffered := false,
    parallelExecution := false,
    testListeners := Seq.empty, // Fix for doubled test reports
    testOptions += Tests.Argument("-oIDOF", "-u", "target/test-reports"),
    testOptions += Tests.Setup(_ => sys.props("sbt-testing") = "true"),
    concurrentRestrictions := {
      val threadNumber = Option(System.getenv("SBT_THREAD_NUMBER")).fold(1)(_.toInt)
      Seq(Tags.limit(Tags.ForkedTestGroup, threadNumber))
    }
  ))

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

/* IDEA notes
 * May require to delete .idea and re-import with all checkboxes
 * Worksheets may not work: https://youtrack.jetbrains.com/issue/SCL-6726
 * To work with worksheets, make sure:
   1. You've selected the appropriate project
   2. You've checked "Make project before run"
 */

import sbt.Keys._
import sbt.{File, IO, Project, _}
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

val langPublishSettings = Seq(
  coverageExcludedPackages := "",
  publishMavenStyle := true,
  publishTo := Some("Sonatype Nexus" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"),
  homepage := Some(url("https://docs.wavesplatform.com/en/technical-details/waves-contracts-language-description/maven-compiler-package.html")),
  developers := List(
    Developer("petermz", "Peter Zhelezniakov", "peterz@rambler.ru", url("https://wavesplatform.com"))
  )
)

lazy val lang =
  crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Full)
    .settings(
      coverageExcludedPackages := ".*",
      test in assembly := {},
      libraryDependencies ++= Dependencies.lang.value ++ Dependencies.test,
      inConfig(Compile)(
        Seq(
          sourceGenerators += Tasks.docSource,
          PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value,
          PB.protoSources := Seq(PB.externalIncludePath.value, baseDirectory.value.getParentFile / "shared" / "src" / "main" / "protobuf"),
          includeFilter in PB.generate := { (f: File) =>
            (** / "DAppMeta.proto").matches(f.toPath) ||
            (** / "waves" / "*.proto").matches(f.toPath)
          },
          PB.deleteTargetDirectory := false
        )
      )
    )

lazy val `lang-jvm` = lang.jvm
  .settings(langPublishSettings)
  .settings(
    name := "RIDE Compiler",
    normalizedName := "lang",
    description := "The RIDE smart contract language compiler",
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.0.0" % Provided
  )

lazy val `lang-js` = lang.js
  .enablePlugins(VersionObject)
  .settings(
    libraryDependencies += Dependencies.circeJsInterop.value
  )

lazy val `lang-testkit` = project
  .dependsOn(`lang-jvm`)
  .in(file("lang/testkit"))
  .settings(langPublishSettings)
  .settings(
    libraryDependencies ++= Dependencies.test.map(_.withConfigurations(Some("compile")))
  )

lazy val `lang-tests` = project.in(file("lang/tests")).dependsOn(`lang-testkit`)

lazy val `lang-doc` = project
  .in(file("lang/doc"))
  .dependsOn(`lang-jvm`)
  .settings(
    libraryDependencies ++= Seq("com.github.spullara.mustache.java" % "compiler" % "0.9.5") ++ Dependencies.test
  )

lazy val node = project.dependsOn(`lang-jvm`, `lang-testkit` % "test")

lazy val `grpc-server`    = project.dependsOn(node % "compile;test->test;runtime->provided")
lazy val `node-it`        = project.dependsOn(node, `grpc-server`)
lazy val `node-generator` = project.dependsOn(node, `node` % "compile")
lazy val benchmark        = project.dependsOn(node % "compile;test->test")

lazy val `curve25519-test` = project.dependsOn(node)

lazy val root = (project in file("."))
  .aggregate(
    `lang-js`,
    `lang-jvm`,
    `lang-tests`,
    `lang-testkit`,
    node,
    `node-it`,
    `node-generator`,
    benchmark
  )

inScope(Global)(
  Seq(
    scalaVersion := "2.13.3",
    organization := "com.wavesplatform",
    organizationName := "Waves Platform",
    V.fallback := (1, 3, 5),
    organizationHomepage := Some(url("https://wavesplatform.com")),
    scmInfo := Some(ScmInfo(url("https://github.com/wavesplatform/Waves"), "git@github.com:wavesplatform/Waves.git", None)),
    licenses := Seq(("MIT", url("https://github.com/wavesplatform/Waves/blob/master/LICENSE"))),
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:postfixOps",
      "-Ywarn-unused:-implicits",
      "-Xlint",
      "-opt:l:inline",
      "-opt-inline-from:**",
      "-Wconf:cat=deprecation&site=com.wavesplatform.api.grpc.*:s" // Ignore gRPC warnings
    ),
    crossPaths := false,
    scalafmtOnCompile := false,
    dependencyOverrides ++= Dependencies.enforcedVersions.value,
    cancelable := true,
    logBuffered := false,
    coverageExcludedPackages := ".*",
    parallelExecution := false,
    testListeners := Seq.empty, // Fix for doubled test reports
    /* http://www.scalatest.org/user_guide/using_the_runner
     * o - select the standard output reporter
     * I - show reminder of failed and canceled tests without stack traces
     * D - show all durations
     * O - drop InfoProvided events
     * F - show full stack traces
     * u - select the JUnit XML reporter with output directory
     */
    testOptions += Tests.Argument("-oIDOF", "-u", "target/test-reports"),
    testOptions += Tests.Setup(_ => sys.props("sbt-testing") = "true"),
    network := Network(sys.props.get("network")),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    sources in (Compile, doc) := Seq.empty,
    publishArtifact in (Compile, packageDoc) := false
  )
)

// ThisBuild options
git.useGitDescribe := true
git.uncommittedSignifier := Some("DIRTY")

lazy val packageAll = taskKey[Unit]("Package all artifacts")
packageAll := {
  (node / assembly).value
  (`grpc-server` / Universal / packageZipTarball).value

  IO.copyFile((node / Debian / packageBin).value, new File(baseDirectory.value, "docker/target/waves.deb"))
  IO.copyFile((`grpc-server` / Debian / packageBin).value, new File(baseDirectory.value, "docker/target/waves-grpc-server.deb"))
}

lazy val checkPRRaw = taskKey[Unit]("Build a project and run unit tests")
checkPRRaw := Def
  .sequential(
    root / clean,
    Def.task {
      (Test / compile).value
      (`lang-tests` / Test / test).value
      (`lang-js` / Compile / fastOptJS).value
      (`grpc-server` / Test / test).value
      (node / Test / test).value
    }
  )
  .value

def checkPR: Command = Command.command("checkPR") { state =>
  val newState = Project
    .extract(state)
    .appendWithoutSession(
      Seq(Global / scalacOptions ++= Seq("-Xfatal-warnings")),
      state
    )
  Project.extract(newState).runTask(checkPRRaw, newState)
  state
}

commands += checkPR

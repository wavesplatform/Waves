/* IDEA notes
 * May require to delete .idea and re-import with all checkboxes
 * Worksheets may not work: https://youtrack.jetbrains.com/issue/SCL-6726
 * To work with worksheets, make sure:
   1. You've selected the appropriate project
   2. You've checked "Make project before run"
 */

import sbt.Keys._
import sbt._
import sbt.internal.inc.ReflectUtilities
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

val langPublishSettings = Seq(
  coverageExcludedPackages := "",
  publishMavenStyle := true,
  credentials += Credentials(Path.userHome / ".sbt" / ".credentials"),
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
          PB.targets += scalapb.gen(flatPackage = true) -> (sourceManaged in Compile).value,
          PB.protoSources := Seq(baseDirectory.value.getParentFile / "shared" / "src" / "main" / "protobuf"),
          PB.deleteTargetDirectory := false
        )
      )
    )

lazy val langJVM = lang.jvm
  .settings(langPublishSettings)
  .settings(
    name := "RIDE Compiler",
    normalizedName := "lang",
    description := "The RIDE smart contract language compiler",
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.0.0" % Provided
  )

lazy val langJS = lang.js
  .enablePlugins(VersionObject)
  .settings(
    libraryDependencies += Dependencies.circeJsInterop.value
  )

lazy val `lang-testkit` = project
  .dependsOn(langJVM)
  .in(file("lang/testkit"))
  .settings(langPublishSettings)
  .settings(
    libraryDependencies ++= Dependencies.test.map(_.withConfigurations(Some("compile")))
  )

lazy val langTests = project.in(file("lang/tests")).dependsOn(`lang-testkit`)

lazy val langDoc = project
  .in(file("lang/doc"))
  .dependsOn(langJVM)
  .settings(
    libraryDependencies ++= Seq("com.github.spullara.mustache.java" % "compiler" % "0.9.5") ++ Dependencies.test
  )

lazy val node             = project.dependsOn(langJVM, `lang-testkit` % "test")
lazy val `grpc-server`    = project.dependsOn(node % "compile;test->test;runtime->provided")
lazy val `node-it`        = project.dependsOn(node, `grpc-server`)
lazy val `node-generator` = project.dependsOn(node, `node-it` % "compile->test")
lazy val benchmark        = project.dependsOn(node % "compile;test->test")

lazy val it = project
  .settings(
    description := "Hack for near future to support builds in TeamCity for old and new branches both",
    Test / test := Def
      .sequential(
        root / packageAll,
        `node-it` / Docker / docker,
        `node-it` / Test / test
      )
      .value
  )

lazy val root = (project in file("."))
  .aggregate(
    langJS,
    langJVM,
    node,
    `node-it`,
    `node-generator`,
    benchmark
  )

inScope(Global)(
  Seq(
    scalaVersion := "2.12.9",
    organization := "com.wavesplatform",
    organizationName := "Waves Platform",
    V.fallback := (1, 1, 10),
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
      "-Ypartial-unification",
      "-opt:l:inline",
      "-opt-inline-from:**"
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
    concurrentRestrictions := {
      val threadNumber = Option(System.getenv("SBT_THREAD_NUMBER")).fold(1)(_.toInt)
      Seq(Tags.limit(Tags.ForkedTestGroup, threadNumber))
    },
    network := Network(sys.props.get("network"))
  )
)

// ThisBuild options
git.useGitDescribe := true
git.uncommittedSignifier := Some("DIRTY")

// root project settings
// https://stackoverflow.com/a/48592704/4050580
def allProjects: List[ProjectReference] = ReflectUtilities.allVals[Project](this).values.toList map { p =>
  p: ProjectReference
}

lazy val cleanAll = taskKey[Unit]("Clean all projects")
cleanAll := clean.all(ScopeFilter(inProjects(allProjects: _*), inConfigurations(Compile))).value

lazy val packageAll = taskKey[Unit]("Package all artifacts")
packageAll := Def
  .sequential(
    root / cleanAll,
    Def.task {
      (node / assembly).value
      (node / Debian / packageBin).value
      (`grpc-server` / Universal / packageZipTarball).value
      (`grpc-server` / Debian / packageBin).value
    }
  )
  .value

lazy val checkPRRaw = taskKey[Unit]("Build a project and run unit tests")
checkPRRaw := {
  try {
    cleanAll.value // Hack to run clean before all tasks
  } finally {
    test.all(ScopeFilter(inProjects(langTests, node), inConfigurations(Test))).value
    (langJS / Compile / fastOptJS).value
    compile.all(ScopeFilter(inProjects(`node-generator`, benchmark, `node-it`), inConfigurations(Test))).value
  }
}

def checkPR: Command = Command.command("checkPR") { state =>
  val updatedState = Project
    .extract(state)
    .appendWithoutSession(Seq(Global / scalacOptions ++= Seq("-Xfatal-warnings", "-Ywarn-unused:-imports")), state)
  Project.extract(updatedState).runTask(root / checkPRRaw, updatedState)
  state
}

commands += checkPR

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
import sbtcrossproject.CrossPlugin.autoImport.crossProject

lazy val common = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .disablePlugins(ProtocPlugin)
  .settings(
    libraryDependencies ++= Dependencies.common.value,
    coverageExcludedPackages := ""
  )

lazy val commonJS  = common.js
lazy val commonJVM = common.jvm

lazy val langSharedSources = settingKey[File]("Shared scala sources of lang module")
lazy val langProtoModels   = settingKey[File]("Protobuf sources of lang module")

lazy val versionSourceSetting = (path: String) => inConfig(Compile)(Seq(sourceGenerators += Tasks.versionSource(path)))

lazy val lang =
  crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .dependsOn(common % "compile;test->test")
    .settings(
      coverageExcludedPackages := ".*",
      test in assembly := {},
      libraryDependencies ++= Dependencies.lang.value ++ Dependencies.test,
      resolvers += Resolver.bintrayIvyRepo("portable-scala", "sbt-plugins"),
      resolvers += Resolver.sbtPluginRepo("releases"),
      //Compile / scalafmt / sourceDirectories += file("shared").getAbsoluteFile / "src" / "main" / "scala" // This doesn't work too
      cleanFiles += langSharedSources.value / "com" / "wavesplatform" / "protobuf",
      inConfig(Compile)(
        Seq(
          PB.targets += scalapb.gen(flatPackage = true) -> langSharedSources.value,
          PB.protoSources := Seq(langProtoModels.value),
          PB.deleteTargetDirectory := false,
          sourceGenerators += Tasks.docSource
        )
      ),
      langSharedSources := baseDirectory.value.getParentFile / "shared" / "src" / "main" / "scala",
      langProtoModels := baseDirectory.value.getParentFile / "shared" / "src" / "main" / "protobuf"
      // Compile / scalafmt / sourceDirectories += file("shared").getAbsoluteFile / "src" / "main" / "scala" // This doesn't work too
    )

lazy val langJS  = lang.js.settings(versionSourceSetting("com.wavesplatform.lang"))
lazy val langJVM = lang.jvm

lazy val node = project
  .dependsOn(
    commonJVM % "compile;test->test",
    langJVM   % "compile;test->test"
  )
  .settings(versionSourceSetting("com.wavesplatform"))

lazy val `grpc-server` = project
  .dependsOn(node % "compile;test->test;runtime->provided")

lazy val `node-it` = project.dependsOn(node)

lazy val `node-generator` = project.dependsOn(node, `node-it` % "compile->test")

lazy val benchmark = project
  .dependsOn(
    node    % "compile;test->test",
    langJVM % "compile;test->test"
  )

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
    commonJS,
    commonJVM,
    langJS,
    langJVM,
    node,
    `node-it`,
    `node-generator`,
    benchmark
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
  ))

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
    test.all(ScopeFilter(inProjects(commonJVM, langJVM, node), inConfigurations(Test))).value
    (commonJS / Compile / fastOptJS).value
    (langJS / Compile / fastOptJS).value
    compile.all(ScopeFilter(inProjects(`node-generator`, benchmark), inConfigurations(Test))).value
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

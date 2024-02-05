/* IDEA notes
 * May require to delete .idea and re-import with all checkboxes
 * Worksheets may not work: https://youtrack.jetbrains.com/issue/SCL-6726
 * To work with worksheets, make sure:
   1. You've selected the appropriate project
   2. You've checked "Make project before run"
 */

Global / onChangedBuildSource := ReloadOnSourceChanges

enablePlugins(GitVersioning)

git.uncommittedSignifier       := Some("DIRTY")
git.useGitDescribe             := true
ThisBuild / git.useGitDescribe := true
ThisBuild / PB.protocVersion   := "3.25.1" // https://protobuf.dev/support/version-support/#java

lazy val lang =
  crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Full)
    .settings(
      assembly / test := {},
      libraryDependencies ++= Dependencies.lang.value ++ Dependencies.test,
      inConfig(Compile)(
        Seq(
          sourceGenerators += Tasks.docSource,
          PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value,
          PB.protoSources += PB.externalIncludePath.value,
          PB.generate / includeFilter := { (f: File) =>
            (** / "waves" / "lang" / "*.proto").matches(f.toPath)
          },
          PB.deleteTargetDirectory := false
        )
      )
    )

lazy val `lang-jvm` = lang.jvm
  .settings(
    name                                  := "RIDE Compiler",
    normalizedName                        := "lang",
    description                           := "The RIDE smart contract language compiler",
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % Provided
  )

lazy val `lang-js` = lang.js
  .enablePlugins(VersionObject)

lazy val `lang-testkit` = project
  .dependsOn(`lang-jvm`)
  .in(file("lang/testkit"))
  .settings(
    libraryDependencies ++=
      Dependencies.test.map(_.withConfigurations(Some("compile"))) ++ Dependencies.qaseReportDeps ++ Dependencies.logDeps ++ Seq(
        "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
      )
  )

lazy val `lang-tests` = project
  .in(file("lang/tests"))
  .dependsOn(`lang-testkit` % "test;test->test")

lazy val `lang-tests-js` = project
  .in(file("lang/tests-js"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(`lang-js`)
  .settings(
    libraryDependencies += Dependencies.scalaJsTest.value,
    testFrameworks += new TestFramework("utest.runner.Framework")
  )

lazy val node = project.dependsOn(`lang-jvm`, `lang-testkit` % "test;test->test")

lazy val `grpc-server`    = project.dependsOn(node % "compile;test->test;runtime->provided")
lazy val `ride-runner`    = project.dependsOn(node % "compile;test->test", `grpc-server`)
lazy val `node-it`        = project.dependsOn(node % "compile;test->test", `lang-testkit`, `repl-jvm`, `grpc-server`)
lazy val `node-generator` = project.dependsOn(node % "compile->test")
lazy val benchmark        = project.dependsOn(node % "compile;test->test")

lazy val repl = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .settings(
    libraryDependencies ++=
      Dependencies.protobuf.value ++
        Dependencies.langCompilerPlugins.value ++
        Dependencies.circe.value,
    inConfig(Compile)(
      Seq(
        PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value,
        PB.protoSources += PB.externalIncludePath.value,
        PB.generate / includeFilter := { (f: File) =>
          (** / "waves" / "*.proto").matches(f.toPath)
        }
      )
    )
  )

lazy val `repl-jvm` = repl.jvm
  .dependsOn(`lang-jvm`, `lang-testkit` % "test;test->test")
  .settings(
    libraryDependencies ++= Dependencies.circe.value ++ Seq(
      "org.scala-js" %% "scalajs-stubs" % "1.1.0" % Provided,
      Dependencies.sttp3
    )
  )

lazy val `repl-js` = repl.js.dependsOn(`lang-js`)
  .settings(
    libraryDependencies += "org.scala-js" %%% "scala-js-macrotask-executor" % "1.1.1"
  )

lazy val `curve25519-test` = project.dependsOn(node)

lazy val `waves-node` = (project in file("."))
  .aggregate(
    `lang-js`,
    `lang-jvm`,
    `lang-tests`,
    `lang-tests-js`,
    `lang-testkit`,
    `repl-js`,
    `repl-jvm`,
    node,
    `node-it`,
    `node-generator`,
    benchmark,
    `repl-js`,
    `repl-jvm`,
    `ride-runner`
  )

inScope(Global)(
  Seq(
    scalaVersion         := "2.13.12",
    organization         := "com.wavesplatform",
    organizationName     := "Waves Platform",
    organizationHomepage := Some(url("https://wavesplatform.com")),
    licenses             := Seq(("MIT", url("https://github.com/wavesplatform/Waves/blob/master/LICENSE"))),
    publish / skip       := true,
    scalacOptions ++= Seq(
      "-Xsource:3",
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:postfixOps",
      "-Ywarn-unused:-implicits",
      "-Xlint",
      "-Wconf:cat=deprecation&site=com.wavesplatform.api.grpc.*:s",                                // Ignore gRPC warnings
      "-Wconf:cat=deprecation&site=com.wavesplatform.protobuf.transaction.InvokeScriptResult.*:s", // Ignore deprecated argsBytes
      "-Wconf:cat=deprecation&site=com.wavesplatform.state.InvokeScriptResult.*:s"
    ),
    crossPaths        := false,
    cancelable        := true,
    parallelExecution := true,
    /* http://www.scalatest.org/user_guide/using_the_runner
     * o - select the standard output reporter
     * I - show reminder of failed and canceled tests without stack traces
     * D - show all durations
     * O - drop InfoProvided events
     * F - show full stack traces
     * u - select the JUnit XML reporter with output directory
     */
    testOptions += Tests.Argument("-oIDOF", "-u", "target/test-reports", "-C", "com.wavesplatform.report.QaseReporter"),
    testOptions += Tests.Setup(_ => sys.props("sbt-testing") = "true"),
    network         := Network.default(),
    instrumentation := false,
    resolvers ++= Resolver.sonatypeOssRepos("releases") ++ Resolver.sonatypeOssRepos("snapshots") ++ Seq(Resolver.mavenLocal),
    Compile / doc / sources                := Seq.empty,
    Compile / packageDoc / publishArtifact := false,
    concurrentRestrictions                 := Seq(Tags.limit(Tags.Test, math.min(EvaluateTask.SystemProcessors, 8))),
    excludeLintKeys ++= Set(
      node / Universal / configuration,
      node / Linux / configuration,
      node / Debian / configuration,
      Global / maxParallelSuites
    )
  )
)

lazy val packageAll = taskKey[Unit]("Package all artifacts")
packageAll := {
  (node / assembly).value
  (`ride-runner` / assembly).value
  buildDebPackages.value
  buildTarballsForDocker.value
}

lazy val buildTarballsForDocker = taskKey[Unit]("Package node and grpc-server tarballs and copy them to docker/target")
buildTarballsForDocker := {
  IO.copyFile(
    (node / Universal / packageZipTarball).value,
    baseDirectory.value / "docker" / "target" / "waves.tgz"
  )
  IO.copyFile(
    (`grpc-server` / Universal / packageZipTarball).value,
    baseDirectory.value / "docker" / "target" / "waves-grpc-server.tgz"
  )
}

lazy val buildRIDERunnerForDocker = taskKey[Unit]("Package RIDE Runner tarball and copy it to docker/target")
buildRIDERunnerForDocker := {
  IO.copyFile(
    (`ride-runner` / Universal / packageZipTarball).value,
    (`ride-runner` / baseDirectory).value / "docker" / "target" / s"${(`ride-runner` / name).value}.tgz"
  )
}

lazy val checkPRRaw = taskKey[Unit]("Build a project and run unit tests")
checkPRRaw := Def
  .sequential(
    `waves-node` / clean,
    Def.task {
      (`lang-tests` / Test / test).value
      (`repl-jvm` / Test / test).value
      (`lang-js` / Compile / fastOptJS).value
      (`lang-tests-js` / Test / test).value
//      (`grpc-server` / Test / test).value
      (node / Test / test).value
      (`repl-js` / Compile / fastOptJS).value
      (`node-it` / Test / compile).value
      (benchmark / Test / compile).value
      (`node-generator` / Compile / compile).value
      (`ride-runner` / Test / compile).value
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

lazy val completeQaseRun = taskKey[Unit]("Complete Qase run")
completeQaseRun := Def.task {
  (`lang-testkit` / Test / runMain).toTask(" com.wavesplatform.report.QaseRunCompleter").value
}.value

lazy val buildDebPackages = taskKey[Unit]("Build debian packages")
buildDebPackages := {
  (`grpc-server` / Debian / packageBin).value
  (node / Debian / packageBin).value
}

def buildPackages: Command = Command("buildPackages")(_ => Network.networkParser) { (state, args) =>
  args.toSet[Network].foreach { n =>
    val newState = Project
      .extract(state)
      .appendWithoutSession(
        Seq(Global / network := n),
        state
      )
    Project.extract(newState).runTask(buildDebPackages, newState)
  }

  Project.extract(state).runTask(packageAll, state)

  state
}

commands ++= Seq(checkPR, buildPackages)

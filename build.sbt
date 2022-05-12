/* IDEA notes
 * May require to delete .idea and re-import with all checkboxes
 * Worksheets may not work: https://youtrack.jetbrains.com/issue/SCL-6726
 * To work with worksheets, make sure:
   1. You've selected the appropriate project
   2. You've checked "Make project before run"
 */

import sbt.Def
import sbt.Keys.{concurrentRestrictions, _}

import scala.collection.Seq

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val lang =
  crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Full)
    .settings(
      assembly / test := {},
      libraryDependencies ++= Dependencies.lang.value ++ Dependencies.test,
      inConfig(Compile)(
        Seq(
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
  .settings(
    Compile / sourceGenerators += Tasks.docSource
  )

lazy val `lang-testkit` = project
  .dependsOn(`lang-jvm`)
  .in(file("lang/testkit"))
  .settings(
    libraryDependencies ++= Dependencies.test.map(_.withConfigurations(Some("compile")))
  )

lazy val `lang-tests` = project
  .in(file("lang/tests"))
  .dependsOn(`lang-testkit`)
  .settings(
    Compile / sourceGenerators += Tasks.docSource
  )

lazy val `lang-doc` = project
  .in(file("lang/doc"))
  .dependsOn(`lang-jvm`)
  .settings(
    Compile / sourceGenerators += Tasks.docSource,
    libraryDependencies ++= Seq("com.github.spullara.mustache.java" % "compiler" % "0.9.10") ++ Dependencies.test
  )

lazy val node = project.dependsOn(`lang-jvm`, `lang-testkit` % "test")

lazy val `grpc-server`    = project.dependsOn(node % "compile;test->test;runtime->provided")
lazy val `node-it`        = project.dependsOn(node, `lang-testkit`, `repl-jvm`, `grpc-server`)
lazy val `node-generator` = project.dependsOn(node)
lazy val benchmark        = project.dependsOn(node % "compile;test->test")

lazy val repl = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .settings(
    libraryDependencies ++=
      Dependencies.protobuf.value ++
        Dependencies.langCompilerPlugins.value ++
        Dependencies.circe.value ++
        Seq(
          "org.scala-js" %%% "scala-js-macrotask-executor" % "1.0.0"
        ),
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
  .dependsOn(`lang-jvm`, `lang-testkit` % "test")
  .settings(
    libraryDependencies ++= Dependencies.circe.value ++ Seq(
      "org.scala-js" %% "scalajs-stubs" % "1.1.0" % Provided,
      Dependencies.sttp3
    )
  )

lazy val `repl-js` = repl.js.dependsOn(`lang-js`)

lazy val `curve25519-test` = project.dependsOn(node)

lazy val `waves-node` = (project in file("."))
  .aggregate(
    `lang-js`,
    `lang-jvm`,
    `lang-tests`,
    `lang-testkit`,
    `repl-js`,
    `repl-jvm`,
    node,
    `node-it`,
    `node-generator`,
    benchmark,
    `repl-js`,
    `repl-jvm`
  )

inScope(Global)(
  Seq(
    scalaVersion         := "2.13.8",
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
    crossPaths := false,
    dependencyOverrides ++= Dependencies.enforcedVersions.value,
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
    testOptions += Tests.Argument("-oIDOF", "-u", "target/test-reports"),
    testOptions += Tests.Setup(_ => sys.props("sbt-testing") = "true"),
    network := Network.default(),
    resolvers ++= Seq(
      Resolver.sonatypeRepo("snapshots"),
      Resolver.mavenLocal
    ),
    Compile / doc / sources                := Seq.empty,
    Compile / packageDoc / publishArtifact := false,
    concurrentRestrictions                 := Seq(Tags.limit(Tags.Test, math.min(EvaluateTask.SystemProcessors, 8)))
  )
)

// ThisBuild options
git.useGitDescribe       := true
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
    `waves-node` / clean,
    Def.task {
      (Test / compile).value
      (`lang-tests` / Test / test).value
      (`repl-jvm` / Test / test).value
      (`lang-js` / Compile / fastOptJS).value
      (`grpc-server` / Test / test).value
      (node / Test / test).value
      (`repl-js` / Compile / fastOptJS).value
      (`node-it` / Test / compile).value
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

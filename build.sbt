import java.nio.file.Files
import sbt.protocol.testing.TestResult

import scala.jdk.CollectionConverters.*

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
ThisBuild / git.useGitDescribe := true
ThisBuild / PB.protocVersion   := Dependencies.gProtoVersion

ThisBuild / dependencyOverrides ++= Dependencies.overrides.value

ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishMavenStyle    := true
ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}

lazy val lang =
  crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Full)
    .settings(
      assembly / test := TestResult.Passed,
      libraryDependencies ++= Dependencies.lang.value ++ Dependencies.test,
      inConfig(Compile)(
        Seq(
          sourceGenerators += Tasks.docSource,
          PB.targets += PB.Target(scalapb.gen(flatPackage = true), sourceManaged.value),
          PB.protoSources += PB.externalIncludePath.value,
          PB.generate / includeFilter := { (f: File) =>
            (** / "waves" / "lang" / "*.proto").matches(f.toPath)
          },
          PB.deleteTargetDirectory := false
        )
      )
    )

lazy val `lang-jvm` = lang.jvm
  .enablePlugins(PublishedModule)
  .settings(
    name           := "RIDE Compiler",
    normalizedName := "lang",
    description    := "The RIDE smart contract language compiler",
    libraryDependencies ++= Seq(
      "org.scala-js" %% "scalajs-stubs" % "1.1.0" % Provided,
      Dependencies.logback,
      Dependencies.scalaLogging,
      Dependencies.gProto,
      Dependencies.gProto % "protobuf"
    )
  )

lazy val `lang-js` = lang.js
  .enablePlugins(VersionObject)
  .settings(
    libraryDependencies ++= Dependencies.scalapbRuntimeJS.value
  )

lazy val `lang-testkit` = project
  .in(file("lang/testkit"))
  .dependsOn(`lang-jvm`)
  .enablePlugins(PublishedModule)
  .settings(
    libraryDependencies ++=
      Dependencies.test.map(_.withConfigurations(Some("compile"))) ++ Dependencies.logDeps :+
        Dependencies.scalaLogging
  )

lazy val `lang-tests` = project
  .in(file("lang/tests"))
  .dependsOn(`lang-testkit`)

lazy val `lang-tests-js` = project
  .in(file("lang/tests-js"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(`lang-js`)
  .settings(
    libraryDependencies += Dependencies.scalaJsTest.value,
    testFrameworks += new TestFramework("utest.runner.Framework")
  )

lazy val node = project.dependsOn(`lang-jvm`)

lazy val `node-testkit` = project
  .in(file("node/testkit"))
  .dependsOn(`node`, `lang-testkit`)
  .enablePlugins(PublishedModule)
  .settings(libraryDependencies ++= Dependencies.nodeTests)

lazy val testTmpDir = settingKey[File]("java.io.tmpdir of forked test JVMs, emptied before every test run")

lazy val `node-tests` = project
  .in(file("node/tests"))
  .dependsOn(`node-testkit`)
  .settings(
    libraryDependencies ++= Dependencies.logDeps,
    // In-process, a leaking or hung suite exhausts the heap of sbt itself, which then thrashes in GC until CI cancels
    // it, leaving nothing to diagnose. A fork confines that to its own JVM and dumps its heap on OOM.
    Test / fork               := true,
    Test / testForkedParallel := true,
    // Uncached: the forwarded -Dlogback.test.level is read from sbt's own JVM, which the action cache does not track
    Test / javaOptions ++= Def.uncached {
      val root = (ThisBuild / baseDirectory).value
      Seq(
        "-Xmx4g",
        "-XX:+HeapDumpOnOutOfMemoryError",
        s"-XX:HeapDumpPath=$root",
        "-XX:+ExitOnOutOfMemoryError",
        s"-XX:ErrorFile=$root/hs_err_pid%p.log",
        "-Dfile.encoding=UTF-8",
        "--add-exports=java.base/sun.nio.ch=ALL-UNNAMED",
        "--add-opens=java.base/java.util.concurrent.atomic=ALL-UNNAMED",
        s"-Djava.io.tmpdir=${(Test / testTmpDir).value}"
      ) ++ sys.props.get("logback.test.level").map(level => s"-Dlogback.test.level=$level")
    },
    // Suites clean up after themselves, but a killed fork (cancel, SIGKILL, ExitOnOutOfMemoryError) cannot: keep its temp files (RocksDB
    // dirs, native libraries) out of the shared /tmp and wipe them before the next run
    Test / testTmpDir := target.value / "test-tmp",
    Test / testOptions += {
      val dir = (Test / testTmpDir).value
      Tests.Setup { () =>
        IO.delete(dir)
        IO.createDirectory(dir)
      }
    }
  )

lazy val `grpc-server` =
  project.dependsOn(node % "compile;runtime->provided", `node-testkit` % "test")

lazy val `ride-runner` = project.dependsOn(node, `grpc-server`, `node-testkit`)
lazy val `node-it`     = project.dependsOn(`repl-jvm`, `grpc-server`, `node-testkit`)

lazy val `node-generator` = project.dependsOn(node, `node-testkit`)

lazy val benchmark = project.dependsOn(node, `node-testkit`)

lazy val repl = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .settings(
    libraryDependencies ++= Dependencies.circe.value ++ Seq(
      Dependencies.protoSchemasLib % "protobuf"
    ),
    inConfig(Compile)(
      Seq(
        PB.targets += PB.Target(scalapb.gen(flatPackage = true), sourceManaged.value),
        PB.protoSources += PB.externalIncludePath.value,
        PB.generate / includeFilter := { (f: File) =>
          (** / "waves" / "*.proto").matches(f.toPath)
        },
        PB.deleteTargetDirectory := false
      )
    )
  )

lazy val `repl-jvm` = repl.jvm
  .dependsOn(`lang-jvm`, `lang-testkit`)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-js" %% "scalajs-stubs" % "1.1.0" % Provided,
      Dependencies.sttp3
    )
  )

lazy val `repl-js` = repl.js
  .dependsOn(`lang-js`)
  .settings(
    libraryDependencies ++= Dependencies.scalapbRuntimeJS.value ++ Seq(
      "org.scala-js" %% "scala-js-macrotask-executor" % "1.1.1"
    )
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
    `node-testkit`,
    `node-tests`,
    `node-generator`,
    `grpc-server`,
    benchmark,
    `ride-runner`
  )

inScope(Global)(
  Seq(
    scalaVersion         := "3.10.0",
    organization         := "com.wavesplatform",
    organizationName     := "Waves Platform",
    organizationHomepage := Some(uri("https://wavesplatform.com")),
    licenses             := Seq(("MIT", uri("https://github.com/wavesplatform/Waves/blob/master/LICENSE"))),
    publish / skip       := true,
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:postfixOps",
      "-Xmax-inlines",
      "50", // Required for FunctionalitySettings compilation
      "-Wunused:all",
      "-Wconf:cat=deprecation&origin=com.wavesplatform.api.grpc.*:s",                                // Ignore gRPC warnings
      "-Wconf:cat=deprecation&origin=com.wavesplatform.protobuf.transaction.InvokeScriptResult.*:s", // Ignore deprecated argsBytes
      "-Wconf:cat=deprecation&origin=com.wavesplatform.state.InvokeScriptResult.*:s",
      "-Wconf:cat=deprecation&origin=com\\.wavesplatform\\.(lang\\..*|JsApiUtils)&origin=com\\.wavesplatform\\.lang\\.v1\\.compiler\\.Terms\\.LET_BLOCK:s",
      "-Wconf:cat=deprecation&msg=ArrowAssoc.* is deprecated since 3\\.10:s", // Deprecated in Scala 3.10, play-json, pureconfig and fastparse macros still expand to it
      "-Wconf:src=src_managed/.*:s"
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
    testOptions += Tests.Argument("-oIDOF", "-u", "target/test-reports"),
    testOptions += Tests.Setup(() => sys.props("sbt-testing") = "true"),
    network := Network.default(),
    resolvers ++= Resolver.sonatypeCentralSnapshots +: Seq(Resolver.mavenLocal),
    Compile / packageDoc / publishArtifact := false,
    // Both limits in one assignment: a Global key is last-writer-wins, and .sbt files load after AutoPlugins, so a
    // second assignment in IntegrationTestsPlugin used to be silently shadowed by this one, making
    // -Dwaves.it.max-parallel-suites a no-op. checkTestLimits guards against that happening again.
    concurrentRestrictions := Seq(
      Tags.limit(Tags.Test, math.min(EvaluateTask.SystemProcessors, 8)),
      Tags.limit(Tags.ForkedTestGroup, maxParallelSuites.value)
    ),
    excludeLintKeys ++= Set(
      node / Universal / configuration,
      node / Linux / configuration,
      node / Debian / configuration,
      Global / maxParallelSuites,
      // sbt-native-packager scope bridges (Debian/Rpm/Universal) nothing here reads; sbt 2's lint reports them, sbt 1's
      // did not. sbt matches this set by key label alone, so one entry per key covers every project and scope.
      daemonGroup,
      daemonGroupGid,
      daemonStdoutLogFile,
      daemonUser,
      daemonUserUid,
      debianControlScriptsDirectory,
      executableScriptName,
      javaOptions,
      name,
      rpmScriptsDirectory,
      sourceDirectory
    )
  )
)

commands += Command.command("packageAll") { state =>
  "node / assembly" :: "ride-runner / assembly" :: "buildDebPackages" :: "stageForDocker" :: state
}

// The image runs on Linux only, so the cross-platform jars the universal package carries are ~90MB it would transfer
// and throw away: conscrypt is the crypto provider for the platforms the image is not, and the Linux-only rocksdb and
// Corretto builds are staged in their place (see stageForDocker below).
lazy val dockerExcludedJars = Seq("AmazonCorrettoCryptoProvider", "conscrypt", "rocksdbjni")

// Corretto ships one jar per platform and the image needs the one matching its own, so they are staged under the
// name docker gives that platform in TARGETARCH and the Dockerfile copies just that directory.
lazy val dockerCorrettoArchs = Map("linux-x86_64" -> "amd64", "linux-aarch_64" -> "arm64")

// A configuration of its own, because a dependency reachable from node - Optional included - is packaged into the
// release tarball and the deb, which ship the cross-platform rocksdb jar on purpose.
lazy val DockerNative = config("docker-native")
ivyConfigurations += DockerNative
libraryDependencies += Dependencies.rocksdbForLinux % DockerNative

lazy val stageForDocker = taskKey[Unit]("Stage node and grpc-server files into docker/target, the image build context")
// Uncached: docker/target is outside sbt's tracked outputs, so a cache hit would skip the copy and leave it stale or empty.
stageForDocker := Def.uncached {
  val dest = baseDirectory.value / "docker" / "target"
  val conv = fileConverter.value

  // Third-party jars change only when a dependency does, so they are staged apart from Waves' own jars and scripts:
  // the image copies each directory as its own layer, and the big one stays cached across code-only builds.
  val universal = ((node / Universal / mappings).value ++ (`grpc-server` / Universal / mappings).value).map { case (ref, path) =>
    conv.toPath(ref).toFile -> path
  }

  val corretto = universal.flatMap { case (file, path) =>
    dockerCorrettoArchs.collectFirst {
      case (classifier, arch) if path.contains("AmazonCorrettoCryptoProvider") && path.contains(s"-$classifier.") =>
        file -> dest / "native" / arch / path.stripPrefix("lib/")
    }
  }

  val rocksdb = update.value
    .select(configurationFilter(DockerNative.name))
    .map(file => file -> dest / "lib" / s"org.rocksdb.${file.getName}")

  val staged = universal
    .filterNot { case (_, path) => dockerExcludedJars.exists(path.contains) }
    .map { case (file, path) =>
      val target =
        if (path.startsWith("lib/") && !path.startsWith("lib/com.wavesplatform.")) dest / "lib" / path.stripPrefix("lib/")
        else dest / "app" / path
      file -> target
    } ++ corretto ++ rocksdb

  val wanted = staged.map(_._2).toSet
  if (dest.isDirectory) {
    val walk = Files.walk(dest.toPath)
    val stale =
      try walk.iterator().asScala.map(_.toFile).filter(f => f.isFile && !wanted.contains(f)).toList
      finally walk.close()
    stale.foreach(IO.delete)
  }

  staged.foreach { case (source, target) =>
    // Copying only what changed keeps BuildKit's context transfer incremental: it detects an unchanged file by
    // size and modification time, and re-sends nothing for one it has already seen.
    if (!target.exists() || target.length() != source.length() || target.lastModified() != source.lastModified())
      IO.copyFile(source, target, preserveLastModified = true)
  }
}

lazy val buildRIDERunnerForDocker = taskKey[Unit]("Package RIDE Runner tarball and copy it to docker/target")
buildRIDERunnerForDocker := Def.uncached {
  IO.copyFile(
    fileConverter.value.toPath((`ride-runner` / Universal / packageZipTarball).value).toFile,
    (`ride-runner` / baseDirectory).value / "docker" / "target" / s"${(`ride-runner` / name).value}.tgz"
  )
}

// The ForkedTestGroup limit is what makes -Dwaves.it.max-parallel-suites mean anything, and it was missing because a
// second Global assignment of concurrentRestrictions shadowed it. Assert it rather than trust a comment.
lazy val checkTestLimits = taskKey[Unit]("Fails if Global/concurrentRestrictions no longer caps forked test groups")
checkTestLimits := Def.uncached {
  val limit     = (Global / maxParallelSuites).value
  val overLimit = Map[ConcurrentRestrictions.Tag, Int](Tags.ForkedTestGroup -> (limit + 1))
  if ((Global / concurrentRestrictions).value.forall(_(overLimit)))
    sys.error(
      s"Global/concurrentRestrictions no longer caps ${Tags.ForkedTestGroup} at $limit - another assignment of that Global key has shadowed it"
    )
  // Forked groups inherit these tags; a Tags.Test entry would cap node-it at the per-processor Tags.Test limit instead.
  val itTags = (`node-it` / Test / testOnly / tags).value ++ (`node-it` / Test / test / tags).value
  if (itTags.exists(_._1 == Tags.Test))
    sys.error(s"node-it test tasks carry ${Tags.Test} again, so its forked suites are capped by that limit, not by maxParallelSuites")
}

lazy val compilePRRaw = taskKey[Unit]("Compile the project")
compilePRRaw := Def.uncached(
  Def
    .sequential(
      clean.all(ScopeFilter(inAnyProject)),
      scalafmtCheck.all(ScopeFilter(inAnyProject, inConfigurations(Compile))),
      compile.all(ScopeFilter(inAnyProject, inConfigurations(Test))),
      checkTestLimits
    )
    .value
)

lazy val checkPRRaw = taskKey[Unit]("Compile the project and run unit tests")
checkPRRaw := Def.uncached(
  Def
    .sequential(
      compilePRRaw,
      Def.sequential(
        // testFull, not test: sbt 2's test is incremental and reports success having run nothing it ran before.
        testFull.all(
          ScopeFilter(inProjects(`lang-tests`, `repl-jvm`, `lang-tests-js`, `grpc-server`, `node-tests`, `ride-runner`), inConfigurations(Test))
        ),
        fullOptJS.all(ScopeFilter(inProjects(`lang-js`, `repl-js`), inConfigurations(Compile))),
        assembly.all(ScopeFilter(inProjects(node))),
        stageForDocker
      )
    )
    .value
)

def commandWithFatalWarnings(commandName: String, task: TaskKey[Unit]): Command =
  Command.command(commandName) { state =>
    val extracted = Project.extract(state)
    val newState = extracted.appendWithoutSession(
      Seq(Global / scalacOptions ++= Seq("-Werror")),
      state
    )

    Project.extract(newState).runTask(task, newState)
    state
  }

def compilePR: Command = commandWithFatalWarnings("compilePR", compilePRRaw)
def checkPR: Command   = commandWithFatalWarnings("checkPR", checkPRRaw)

commands += Command.command("buildDebPackages") { state =>
  "set node / Debian / packageArchitecture := \"arm64\"" ::
    "node/ Debian / packageBin" ::
    "set node / Debian / packageArchitecture := \"amd64\"" ::
    "node / Debian / packageBin" ::
    "grpc-server / Debian / packageBin" ::
    state
}

lazy val buildPlatformIndependentArtifacts = taskKey[Unit]("Build fat JARs for node and ride-runner and TGZ for grpc-server")
buildPlatformIndependentArtifacts := Def.uncached {
  (node / assembly).value
  (`ride-runner` / assembly).value
  (`grpc-server` / Universal / packageZipTarball).value
}

commands += Command("buildReleaseArtifacts")(_ => Network.networkParser) { (state, args) =>
  args.toSet[Network].toList.flatMap { n =>
    s"set Global / network := $n" :: "buildDebPackages" :: Nil
  } ::: "buildPlatformIndependentArtifacts" :: state
}

/** Command: generateGenesis <path-to-config>
  * Runs: node / runMain com.wavesplatform.GenesisBlockGenerator <path>
  * Path is always resolved relative to build root, output without "[info]".
  */
def generateGenesisCommand: Command =
  Command.single("generateGenesis") { (state, rawPath) =>
    val ex = Project.extract(state)

    val rootBase = ex.get(LocalRootProject / baseDirectory)
    val absFile = {
      val f = file(rawPath)
      if (f.isAbsolute) f else rootBase / rawPath
    }

    val stateWithSettings = ex.appendWithoutSession(
      Seq(
        ThisBuild / useSuperShell             := false,
        node / Compile / run / outputStrategy := Some(StdoutOutput),
        node / Compile / run / logLevel       := Level.Error
      ),
      state
    )

    val input = s" com.wavesplatform.GenesisBlockGenerator ${absFile.getAbsolutePath}"

    Project
      .extract(stateWithSettings)
      .runInputTask(node / Compile / runMain, input, stateWithSettings)

    state
  }

commands ++= Seq(compilePR, checkPR, generateGenesisCommand)

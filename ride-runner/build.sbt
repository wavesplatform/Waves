name        := "waves-ride-runner"
description := "Allows to execute RIDE code independently from Waves NODE"

enablePlugins(
  JavaServerAppPackaging,
  UniversalDeployPlugin,
  JDebPackaging,
  SystemdPlugin,
  JavaAgent
)

libraryDependencies ++= Dependencies.rideRunner.value

inConfig(Compile)(
  Seq(
    // Affects sbt-native-packager
    mainClass                    := Some("com.wavesplatform.ride.runner.entrypoints.WavesRideRunnerWithBlockchainService"),
    packageDoc / publishArtifact := false,
    packageSrc / publishArtifact := false
  )
)

def mkJavaOptions(forPackager: Boolean = false, extraOptions: Seq[String] = Seq.empty): Seq[String] = {
  val options = Seq("lang", "math", "util").map(x => s"--add-opens=java.base/java.$x=ALL-UNNAMED") ++ // for ehcache/sizeoOf
    Seq(
      "-server",
      "-XX:+ExitOnOutOfMemoryError",
      "-XX:+UseG1GC",
      "-XX:+ParallelRefProcEnabled",
      "-XX:+UseStringDeduplication",
      // Required for GCLockerRetryAllocationCount, otherwise we get:
      //   Error: VM option 'GCLockerRetryAllocationCount' is diagnostic and must be enabled via -XX:+UnlockDiagnosticVMOptions.
      //   Error: The unlock option must precede 'GCLockerRetryAllocationCount'.
      "-XX:+UnlockDiagnosticVMOptions",
      "-XX:GCLockerRetryAllocationCount=100", // prevents false OOM during native calls
      "-Djdk.attach.allowAttachSelf=true",    // for ehcache/sizeoOf
      // JVM default charset for proper and deterministic getBytes behaviour
      "-Dfile.encoding=UTF-8"
    ) ++ extraOptions

  if (forPackager) options.map(x => s"-J$x") else options
}

run / javaOptions ++= mkJavaOptions()
run / fork := true // For working instrumentation

Test / javaOptions ++= mkJavaOptions()
Test / fork := true

bashScriptExtraDefines += bashScriptEnvConfigLocation.value.fold("")(envFile => s"[[ -f $envFile ]] && . $envFile")

linuxScriptReplacements += ("network" -> network.value.toString)

// Causes "OpenJDK 64-Bit Server VM warning: Sharing is only supported for boot loader classes because bootstrap classpath has been appended".
// May ignore
javaAgents ++= Dependencies.kanela

inConfig(Universal)(
  Seq(
    mappings ++=
      (baseDirectory.value / "doc").listFiles().map { x => (x, s"doc/${x.getName}") }.toSeq ++
        Seq(
          (Compile / resourceDirectory).value / "logback.xml" -> "doc/logback.xml",
          (baseDirectory.value / "README.md")                 -> "doc/README.md"
        ),
    javaOptions ++= mkJavaOptions(
      forPackager = true,
      Seq(
        "-Xmx512m",
        "-XX:MaxMetaspaceSize=152m",
        "-XX:ThreadStackSize=1024" // Update the metrics if you change -XX:ThreadStackSize=1024 (1 KiB)
      )
    )
  )
)

inConfig(Debian)(
  Seq(
    maintainer               := "com.wavesplatform",
    packageSource            := sourceDirectory.value / "package",
    linuxStartScriptTemplate := (packageSource.value / "systemd.service").toURI.toURL,
    debianPackageDependencies += "java11-runtime-headless",
    maintainerScripts := maintainerScriptsFromDirectory(packageSource.value / "debian", Seq("postinst", "postrm", "prerm"))
  )
)

// Fat JAR settings
inTask(assembly)(
  Seq(
    test            := {},
    mainClass       := Some("com.wavesplatform.ride.runner.entrypoints.WavesRideRunnerWithPreparedStateApp"),
    assemblyJarName := s"waves-ride-runner-all-${version.value}.jar",
    assemblyMergeStrategy := {
      case p
          if p.endsWith(".proto") ||
            p.endsWith("module-info.class") ||
            p.endsWith("io.netty.versions.properties") =>
        MergeStrategy.discard

      case "scala-collection-compat.properties" =>
        MergeStrategy.discard
      case p
          if Set("scala/util/control/compat", "scala/collection/compat")
            .exists(p.replace('\\', '/').contains) =>
        MergeStrategy.last

      case "logback.xml" | "swagger-ui/openapi.yaml" => MergeStrategy.last

      case other => (assembly / assemblyMergeStrategy).value(other)
    }
  )
)

name        := "ride-runner"
description := "Allows to execute RIDE code independently from Waves NODE"

mainClass := Some("com.wavesplatform.ride.runner.entrypoints.RideRunnerWithPreparedStateApp")

enablePlugins(
  JavaServerAppPackaging,
  UniversalDeployPlugin,
  GitVersioning,
  JavaAgent
)

libraryDependencies ++= Dependencies.rideRunner.value

// Causes "OpenJDK 64-Bit Server VM warning: Sharing is only supported for boot loader classes because bootstrap classpath has been appended".
// May ignore
javaAgents ++= Dependencies.kanela

inConfig(Compile)(
  Seq(
    packageDoc / publishArtifact := false,
    packageSrc / publishArtifact := false
  )
)

val commonJavaOptions = Seq(
  // JVM default charset for proper and deterministic getBytes behaviour
  "-Dfile.encoding=UTF-8"
) ++ Seq("lang", "math", "util").map(x => s"--add-opens=java.base/java.$x=ALL-UNNAMED") // For ehcache

run / javaOptions ++= commonJavaOptions
run / fork := true // For working instrumentation

Test / javaOptions ++= commonJavaOptions
Test / fork := true

bashScriptExtraDefines += bashScriptEnvConfigLocation.value.fold("")(envFile => s"[[ -f $envFile ]] && . $envFile")

linuxScriptReplacements += ("network" -> network.value.toString)

inConfig(Universal)(
  Seq(
    mappings ++= Seq(
      baseDirectory.value / "ride-runner-sample.conf" -> "doc/ride-runner.conf.sample",
      // Logback doesn't allow .xml.sample. Need this, because we have node logback.xml in classpath too
      (Compile / resourceDirectory).value / "logback.xml" -> "doc/logback.sample.xml"
    ),
    javaOptions ++= Seq(
      // -J prefix is required by the bash script
      "-J-server",
      "-J-Xmx2g",
      "-J-XX:+ExitOnOutOfMemoryError",
      "-J-XX:+UseG1GC",
      "-J-XX:+ParallelRefProcEnabled",
      "-J-XX:+UseStringDeduplication"
    ) ++ commonJavaOptions
  )
)

// Fat JAR settings
inTask(assembly)(
  Seq(
    test            := {},
    assemblyJarName := s"ride-runner-all-${version.value}.jar",
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

      case "logback.xml" => MergeStrategy.last

      case other => (assembly / assemblyMergeStrategy).value(other)
    }
  )
)

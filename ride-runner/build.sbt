name        := "ride-runner"
description := "Allows to execute RIDE code independently from Waves NODE"

mainClass := Some("com.wavesplatform.ride.app.RideWithBlockchainUpdatesService")
//discoveredMainClasses := (Compile / mainClass).value.toSeq
run / fork := true // For working instrumentation

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

Test / javaOptions += "-Djol.magicFieldOffset=true" // Cannot get the field offset
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
      "-J-XX:+UseStringDeduplication",
      // JVM default charset for proper and deterministic getBytes behaviour
      "-J-Dfile.encoding=UTF-8",
      "--add-opens=java.base/java.lang=ALL-UNNAMED"
    )
  )
)

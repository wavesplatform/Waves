name        := "ride-runner"
description := "Allows to execute RIDE code independently from Waves NODE"

mainClass := Some("com.wavesplatform.ride.app.RideWithBlockchainUpdatesService")
//discoveredMainClasses := (Compile / mainClass).value.toSeq
run / fork := true

enablePlugins(
  JavaServerAppPackaging,
  UniversalDeployPlugin,
  GitVersioning,
  JavaAgent
)

libraryDependencies ++= Dependencies.rideRunner.value

javaAgents ++= {
  if (instrumentation.value) {
    Dependencies.kanela
  } else {
    Seq.empty
  }
}

inConfig(Compile)(
  Seq(
    packageDoc / publishArtifact := false,
    packageSrc / publishArtifact := false
  )
)

bashScriptExtraDefines += bashScriptEnvConfigLocation.value.fold("")(envFile => s"[[ -f $envFile ]] && . $envFile")

linuxScriptReplacements += ("network" -> network.value.toString)

inConfig(Universal)(
  Seq(
    mappings ++= Seq(
      baseDirectory.value / "ride-runner-sample.conf" -> "doc/ride-runner.conf.sample",
      resourceDirectory.value / "logback.xml"         -> "doc/logback.xml"
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
      "-J-Dfile.encoding=UTF-8"
    )
  )
)

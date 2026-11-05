import sbt.*
import sbt.Keys.*

import scala.sys.process.*

enablePlugins(IntegrationTestsPlugin)

description := "NODE integration tests"
libraryDependencies ++= Dependencies.it

val docker = taskKey[Unit]("Build docker image for integration tests")
// Uncached: the image lives in the docker daemon, where the action cache cannot see it.
docker := Def.uncached {
  val log = streams.value.log

  val cwd   = baseDirectory.value.getParentFile / "docker"
  val image = "com.wavesplatform/node-it:latest"

  val cmd = Seq("docker", "build", "-t", image, ".")
  log.info(s"Running `${cmd.mkString(" ")}` from $cwd")

  val processLogger = ProcessLogger(
    (out: String) => log.info(out),
    (err: String) => log.info(err) // Redirect STDERR to info
  )

  val exit = Process(cmd, cwd).!(processLogger)
  if (exit != 0) sys.error(s"Docker build failed with exit code $exit")
}

val stageForDocker = taskKey[Unit]("stage node files into the docker build context")

// To solve "Error response from daemon: No such image: " see:
// https://github.com/marcus-drake/sbt-docker/issues/133#issuecomment-2718354260
docker := docker.dependsOn(LocalProject("waves-node") / stageForDocker).value

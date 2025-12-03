enablePlugins(IntegrationTestsPlugin, sbtdocker.DockerPlugin)

description := "NODE integration tests"
libraryDependencies ++= Dependencies.it

inTask(docker)(
  Seq(
    imageNames   := Seq(ImageName("com.wavesplatform/node-it")),
    dockerfile   := NativeDockerfile(baseDirectory.value.getParentFile / "docker" / "Dockerfile"),
    buildOptions := BuildOptions()
  )
)

val buildTarballsForDocker = taskKey[Unit]("build all packages")

// To solve "Error response from daemon: No such image: " see:
// https://github.com/marcus-drake/sbt-docker/issues/133#issuecomment-2718354260
docker := docker.dependsOn(LocalProject("waves-node") / buildTarballsForDocker).value
